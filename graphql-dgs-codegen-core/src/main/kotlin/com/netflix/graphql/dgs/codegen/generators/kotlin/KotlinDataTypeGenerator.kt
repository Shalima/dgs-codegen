/*
 *
 *  Copyright 2020 Netflix, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package com.netflix.graphql.dgs.codegen.generators.kotlin

import com.netflix.graphql.dgs.codegen.CodeGenConfig
import com.netflix.graphql.dgs.codegen.CodeGenResult
import com.netflix.graphql.dgs.codegen.filterSkipped
import com.netflix.graphql.dgs.codegen.generators.java.InputTypeGenerator
import com.netflix.graphql.dgs.codegen.shouldSkip
import com.squareup.kotlinpoet.AnnotationSpec
import com.squareup.kotlinpoet.BOOLEAN
import com.squareup.kotlinpoet.ClassName
import com.squareup.kotlinpoet.CodeBlock
import com.squareup.kotlinpoet.DOUBLE
import com.squareup.kotlinpoet.FLOAT
import com.squareup.kotlinpoet.FileSpec
import com.squareup.kotlinpoet.FunSpec
import com.squareup.kotlinpoet.INT
import com.squareup.kotlinpoet.KModifier
import com.squareup.kotlinpoet.MemberName
import com.squareup.kotlinpoet.ParameterSpec
import com.squareup.kotlinpoet.ParameterizedTypeName
import com.squareup.kotlinpoet.PropertySpec
import com.squareup.kotlinpoet.STRING
import com.squareup.kotlinpoet.TypeSpec
import graphql.language.*
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.lang.IllegalArgumentException
import com.squareup.kotlinpoet.TypeName as KtTypeName

class KotlinDataTypeGenerator(config: CodeGenConfig, document: Document) :
    AbstractKotlinDataTypeGenerator(packageName = config.packageNameTypes, config = config, document = document) {

    companion object {
        private val logger: Logger = LoggerFactory.getLogger(KotlinDataTypeGenerator::class.java)
    }

    fun generate(definition: ObjectTypeDefinition, extensions: List<ObjectTypeExtensionDefinition>): CodeGenResult {
        if (definition.shouldSkip(config)) {
            return CodeGenResult()
        }

        logger.info("Generating data type {}", definition.name)

        val fields = definition.fieldDefinitions
            .filterSkipped()
            .filter(ReservedKeywordFilter.filterInvalidNames)
            .map { Field(it.name, typeUtils.findReturnType(it.type), typeUtils.isNullable(it.type), null, it.description, it.directives) } +
            extensions.flatMap { it.fieldDefinitions }
                .filterSkipped()
                .map { Field(it.name, typeUtils.findReturnType(it.type), typeUtils.isNullable(it.type), null, it.description, it.directives) }
        val interfaces = definition.implements
        return generate(definition.name, fields, interfaces, document, definition.description, definition.directives)
    }

    override fun getPackageName(): String {
        return config.packageNameTypes
    }
}

class KotlinInputTypeGenerator(config: CodeGenConfig, document: Document) :
    AbstractKotlinDataTypeGenerator(packageName = config.packageNameTypes, config = config, document = document) {
    private val logger: Logger = LoggerFactory.getLogger(InputTypeGenerator::class.java)

    fun generate(definition: InputObjectTypeDefinition, extensions: List<InputObjectTypeExtensionDefinition>): CodeGenResult {
        if (definition.shouldSkip(config)) {
            return CodeGenResult()
        }

        logger.info("Generating input type ${definition.name}")

        val fields = definition.inputValueDefinitions
            .filter(ReservedKeywordFilter.filterInvalidNames)
            .map {
                val type = typeUtils.findReturnType(it.type)
                val defaultValue = it.defaultValue?.let { value -> generateCode(value, type) }
                Field(name = it.name, type = type, nullable = typeUtils.isNullable(it.type), default = defaultValue, description = it.description)
            }.plus(
                extensions.flatMap { it.inputValueDefinitions }.map {
                    Field(name = it.name, type = typeUtils.findReturnType(it.type), nullable = typeUtils.isNullable(it.type), default = null, description = it.description)
                }
            )
        val interfaces = emptyList<Type<*>>()
        return generate(definition.name, fields, interfaces, document, definition.description, definition.directives)
    }

    private fun generateCode(value: Value<Value<*>>, type: KtTypeName): CodeBlock =
        when (value) {
            is BooleanValue -> CodeBlock.of("%L", value.isValue)
            is IntValue -> CodeBlock.of("%L", value.value)
            is StringValue -> CodeBlock.of("%S", value.value)
            is FloatValue -> CodeBlock.of("%L", value.value)
            is EnumValue -> CodeBlock.of("%M", MemberName(type.className, value.name))
            is ArrayValue ->
                if (value.values.isEmpty()) CodeBlock.of("emptyList()")
                else CodeBlock.of("listOf(%L)", value.values.joinToString { v -> generateCode(v, type).toString() })
            else -> CodeBlock.of("%L", value)
        }

    private val KtTypeName.className: ClassName
        get() = when (this) {
            is ClassName -> this
            is ParameterizedTypeName -> typeArguments[0].className
            else -> TODO()
        }

    override fun getPackageName(): String {
        return config.packageNameTypes
    }
}

internal data class Field(
    val name: String,
    val type: KtTypeName,
    val nullable: Boolean,
    val default: CodeBlock? = null,
    val description: Description? = null,
    val directives: List<Directive> = emptyList()
)

abstract class AbstractKotlinDataTypeGenerator(packageName: String, protected val config: CodeGenConfig, protected val document: Document) {
    protected val typeUtils = KotlinTypeUtils(
        packageName = packageName,
        config = config,
        document = document
    )

    private fun generateCode(value: Value<Value<*>>, className: ClassName?, prefix: String? = ""): CodeBlock =
        when (value) {
            is BooleanValue -> CodeBlock.of("$prefix%L", (value as BooleanValue).isValue)
            is IntValue -> CodeBlock.of("$prefix%L", (value as IntValue).value)
            is StringValue -> CodeBlock.of("$prefix%S", (value as StringValue).value)
            is FloatValue -> CodeBlock.of("$prefix%L", (value as FloatValue).value)
            is EnumValue -> CodeBlock.of("$prefix%M", className?.let { MemberName(it, (value as EnumValue).name) })
            is ArrayValue ->
                if ((value as ArrayValue).values.isEmpty()) CodeBlock.of("emptyList()")
                else CodeBlock.of("$prefix[%L]", (value as ArrayValue).values.joinToString { v -> generateCode(v, className).toString() })
            else -> CodeBlock.of("$prefix%L", value)
        }

    private fun createAnnotations(directives: List<Directive>): MutableList<AnnotationSpec> {
        var annotations: MutableList<AnnotationSpec> = mutableListOf()
        directives.forEach { directive ->
            if (directive.name == "validate") {
                annotations.add(createAnnotation(directive, config.includeImports.getOrDefault("validatorPackage", "")))
            } else {
                throw IllegalArgumentException("Unknown directive")
            }
        }
        return annotations
    }

    private fun createAnnotation(directive: Directive, configPackageName: String): AnnotationSpec {
        if (directive.arguments.isEmpty() || directive.arguments[0].name != "name") {
            throw IllegalArgumentException("Invalid validate directive")
        }
        val wholePackageName = (directive.arguments[0].value as StringValue).value
        val packageName = if (wholePackageName.substringBeforeLast(".", "").isEmpty()) configPackageName else wholePackageName.substringBeforeLast(".")
        val simpleName = wholePackageName.substringAfterLast(".")
        val className: ClassName = ClassName(packageName = packageName, simpleNames = listOf(simpleName))
        val annotation: AnnotationSpec.Builder = AnnotationSpec.builder(className)
        if (directive.arguments.size > 1) {
            directive.arguments.drop(1).forEach { argument ->
                val className: ClassName? = if (argument.value is EnumValue) ClassName(packageName = (argument.value as EnumValue).name, simpleNames = listOf((argument.value as EnumValue).name)) else null
                val codeBlock: CodeBlock = generateCode(argument.value, className, argument.name + " = ")
                annotation.addMember(codeBlock)
            }
        }
        return annotation.build()
    }

    private fun applyDirectives(directives: List<Directive>, parameterSpec: ParameterSpec.Builder) {
        parameterSpec.addAnnotations(createAnnotations(directives))
    }

    private fun applyDirectives(directives: List<Directive>, typeSpec: TypeSpec.Builder) {
        typeSpec.addAnnotations(createAnnotations(directives))
    }

    internal fun generate(
        name: String,
        fields: List<Field>,
        interfaces: List<Type<*>>,
        document: Document,
        description: Description? = null,
        directives: List<Directive> = emptyList()
    ): CodeGenResult {
        val kotlinType = TypeSpec.classBuilder(name)

        if (fields.isNotEmpty()) {
            kotlinType.addModifiers(KModifier.DATA)
        }

        if (description != null) {
            kotlinType.addKdoc("%L", description.sanitizeKdoc())
        }

        if (directives.isNotEmpty()) {
            applyDirectives(directives, kotlinType)
        }

        val constructorBuilder = FunSpec.constructorBuilder()

        fields.forEach { field ->
            val returnType = if (field.nullable) field.type.copy(nullable = true) else field.type
            val parameterSpec = ParameterSpec.builder(field.name, returnType)
                .addAnnotation(jsonPropertyAnnotation(field.name))

            if (field.directives.isNotEmpty()) {
                applyDirectives(field.directives, parameterSpec)
            }

            if (field.default != null) {
                parameterSpec.defaultValue(field.default)
            } else {
                when (returnType) {
                    STRING -> if (field.nullable) parameterSpec.defaultValue("null")
                    INT -> if (field.nullable) parameterSpec.defaultValue("null")
                    FLOAT -> if (field.nullable) parameterSpec.defaultValue("null")
                    DOUBLE -> if (field.nullable) parameterSpec.defaultValue("null")
                    BOOLEAN -> if (field.nullable) parameterSpec.defaultValue("null")
                    else -> if (field.nullable) parameterSpec.defaultValue("null")
                }
            }

            val interfaceNames = interfaces.asSequence()
                .map { it as NamedNode<*> }
                .map { it.name }
                .toSet()
            val interfaceTypes = document.getDefinitionsOfType(InterfaceTypeDefinition::class.java)
            val implementedInterfaces = interfaceTypes.filter { interfaceNames.contains(it.name) }
            val interfaceFields = implementedInterfaces.asSequence()
                .flatMap { it.fieldDefinitions }
                .map { it.name }
                .toSet()

            if (field.name in interfaceFields) {
                parameterSpec.addModifiers(KModifier.OVERRIDE)
            }

            constructorBuilder.addParameter(parameterSpec.build())
            val propertySpecBuilder = PropertySpec.builder(field.name, returnType)
            if (field.description != null) {
                propertySpecBuilder.addKdoc("%L", field.description.sanitizeKdoc())
            }
            propertySpecBuilder.initializer(field.name)
            kotlinType.addProperty(propertySpecBuilder.build())
        }

        val unionTypes = document.getDefinitionsOfType(UnionTypeDefinition::class.java).filter { union ->
            union.memberTypes.asSequence().map { it as TypeName }.any { it.name == name }
        }

        val interfaceTypes = interfaces + unionTypes
        interfaceTypes.forEach {
            if (it is NamedNode<*>) {
                kotlinType.addSuperinterface(ClassName.bestGuess("${getPackageName()}.${it.name}"))
            }
        }

        if (interfaceTypes.isNotEmpty()) {
            kotlinType.addAnnotation(disableJsonTypeInfoAnnotation())
        }

        kotlinType.primaryConstructor(constructorBuilder.build())
        kotlinType.addType(TypeSpec.companionObjectBuilder().build())

        val typeSpec = kotlinType.build()

        val fileSpec = FileSpec.builder(getPackageName(), typeSpec.name!!).addType(typeSpec).build()

        return CodeGenResult(kotlinDataTypes = listOf(fileSpec))
    }

    abstract fun getPackageName(): String
}

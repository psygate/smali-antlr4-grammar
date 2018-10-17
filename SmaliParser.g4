//MIT License
//
//Copyright (c) 2018 psygate
//
//Permission is hereby granted, free of charge, to any person obtaining a copy
//of this software and associated documentation files (the "Software"), to deal
//in the Software without restriction, including without limitation the rights
//to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//copies of the Software, and to permit persons to whom the Software is
//furnished to do so, subject to the following conditions:
//
//The above copyright notice and this permission notice shall be included in all
//copies or substantial portions of the Software.
//
//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//SOFTWARE.

parser grammar SmaliParser;

options {
	tokenVocab=SmaliLexer;
}

// Helpers

registerIdentifier:             IDENTIFIER;
stringLiteral:                  STRING_LITERAL;
negativeNumericLiteral:         SUB positiveNumericLiteral;

positiveNumericLiteral:
    DECIMAL_LITERAL
    | HEX_LITERAL
    | OCT_LITERAL
    | BINARY_LITERAL
    | FLOAT_LITERAL
    | HEX_FLOAT_LITERAL
    ;

numericLiteral:                 negativeNumericLiteral | positiveNumericLiteral;

// This rule is necessary, since some keywords like "annotation" aren't context free.
identifier:
    (
        IDENTIFIER
        | VOID_TYPE
        | BOOLEAN_TYPE
        | BYTE_TYPE
        | SHORT_TYPE
        | CHAR_TYPE
        | INT_TYPE
        | LONG_TYPE
        | FLOAT_TYPE
        | DOUBLE_TYPE
        | CONSTRUCTOR
        | ANNOTATION
        | BRIDGE
        | NATIVE
        | VARARGS
        | SYNTHETIC
    )+
    ;

// Types
referenceType:                  QUALIFIED_TYPE_NAME;
voidType:                       VOID_TYPE;
booleanType:                    BOOLEAN_TYPE;
byteType:                       BYTE_TYPE;
shortType:                      SHORT_TYPE;
charType:                       CHAR_TYPE;
intType:                        INT_TYPE;
longType:                       LONG_TYPE;
floatType:                      FLOAT_TYPE;
doubleType:                     DOUBLE_TYPE;

primitiveType:
    booleanType
    | byteType
    | shortType
    | charType
    | intType
    | longType
    | floatType
    | doubleType
    ;

nonArrayType:               primitiveType | referenceType;

methodParameterLiteral:     (COMPOUND_METHOD_ARG_LITERAL | IDENTIFIER);     // strings like IIS

arrayType:                  LBRACK (nonArrayType | arrayType | methodParameterLiteral);

referenceOrArrayType:       referenceType | arrayType;

nonVoidType:                nonArrayType | arrayType;

anyType:                    nonVoidType | voidType | arrayType;

nullLiteral:                NULL_LITERAL;

booleanLiteral:             BOOL_LITERAL;

assignableValue:            anyType | stringLiteral | numericLiteral | nullLiteral | booleanLiteral;

// Modifiers

classModifier:
    PUBLIC
    | PRIVATE
    | PROTECTED
    | FINAL
    | ANNOTATION
    | SYNTHETIC
    | STATIC
    | ABSTRACT
    | ENUM
    | INTERFACE
    ;

methodModifier:
    PUBLIC
    | PRIVATE
    | PROTECTED
    | FINAL
    | SYNTHETIC
    | STATIC
    | ABSTRACT
    | CONSTRUCTOR
    | BRIDGE
    | DECLARED_SYNCHRONIZED
    | STRICTFP
    | VARARGS
    | NATIVE
    ;

fieldModifier:
    PUBLIC
    | PRIVATE
    | PROTECTED
    | FINAL
    | SYNTHETIC
    | STATIC
    | TRANSIENT
    | VOLATILE
    | ENUM
    ;

labelName:                      identifier;

label:                          COLON labelName;

// Instructions

leftRegister:                   registerIdentifier;

rightRegister:                  registerIdentifier;

registerListRegisters:          registerIdentifier (COMMA registerIdentifier)*;

registerRange:                  LBRACE leftRegister DOT DOT rightRegister RBRACE;

registerList:                   LBRACE RBRACE | LBRACE registerListRegisters RBRACE;

operand:
    (
        registerIdentifier
        | numericLiteral
        | stringLiteral
        | nonVoidType
        | registerList
        | methodInvocationTarget
        | label
        | registerRange
        | fieldInvocationTarget
        | methodParameterLiteral
    );

nullaryInstruction:
    OP_RETURN_VOID
    | OP_NOP
    ;

binaryInstruction:
    (
        OP_MOVE
        | OP_MOVE_FROM16
        | OP_MOVE_16
        | OP_MOVE_WIDE
        | OP_MOVE_WIDE_FROM16
        | OP_MOVE_WIDE_16
        | OP_MOVE_OBJECT
        | OP_MOVE_OBJECT_FROM16
        | OP_MOVE_OBJECT_16

        | OP_CONST_4
        | OP_CONST_16
        | OP_CONST
        | OP_CONST_HIGH16
        | OP_CONST_WIDE_16
        | OP_CONST_WIDE_32
        | OP_CONST_WIDE
        | OP_CONST_WIDE_HIGH16

        | OP_CONST_STRING
        | OP_CONST_STRING_JUMBO

        | OP_CONST_CLASS

        | OP_CHECK_CAST
        | OP_ARRAY_LENGTH
        | OP_NEW_INSTANCE

        | OP_FILLED_NEW_ARRAY_RANGE

        | OP_SGET
        | OP_SGET_WIDE
        | OP_SGET_OBJECT
        | OP_SGET_BOOLEAN
        | OP_SGET_BYTE
        | OP_SGET_CHAR
        | OP_SGET_SHORT

        | OP_SPUT
        | OP_SPUT_WIDE
        | OP_SPUT_OBJECT
        | OP_SPUT_BOOLEAN
        | OP_SPUT_BYTE
        | OP_SPUT_CHAR
        | OP_SPUT_SHORT

        | OP_INVOKE_VIRTUAL
        | OP_INVOKE_SUPER
        | OP_INVOKE_DIRECT
        | OP_INVOKE_STATIC
        | OP_INVOKE_INTERFACE

        | OP_INVOKE_VIRTUAL_RANGE
        | OP_INVOKE_SUPER_RANGE
        | OP_INVOKE_DIRECT_RANGE
        | OP_INVOKE_STATIC_RANGE
        | OP_INVOKE_INTERFACE_RANGE

        | OP_NEG_INT
        | OP_NOT_INT
        | OP_NEG_LONG
        | OP_NOT_LONG
        | OP_NEG_FLOAT
        | OP_NEG_DOUBLE
        | OP_INT_TO_LONG
        | OP_INT_TO_FLOAT
        | OP_INT_TO_DOUBLE
        | OP_LONG_TO_INT
        | OP_LONG_TO_FLOAT
        | OP_LONG_TO_DOUBLE
        | OP_FLOAT_TO_INT
        | OP_FLOAT_TO_LONG
        | OP_FLOAT_TO_DOUBLE
        | OP_DOUBLE_TO_INT
        | OP_DOUBLE_TO_LONG
        | OP_DOUBLE_TO_FLOAT
        | OP_INT_TO_BYTE
        | OP_INT_TO_CHAR
        | OP_INT_TO_SHORT

        | OP_ADD_INT_2ADDR
        | OP_SUB_INT_2ADDR
        | OP_MUL_INT_2ADDR
        | OP_DIV_INT_2ADDR
        | OP_REM_INT_2ADDR
        | OP_AND_INT_2ADDR
        | OP_OR_INT_2ADDR
        | OP_XOR_INT_2ADDR
        | OP_SHL_INT_2ADDR
        | OP_SHR_INT_2ADDR
        | OP_USHR_INT_2ADDR
        | OP_ADD_LONG_2ADDR
        | OP_SUB_LONG_2ADDR
        | OP_MUL_LONG_2ADDR
        | OP_DIV_LONG_2ADDR
        | OP_REM_LONG_2ADDR
        | OP_AND_LONG_2ADDR
        | OP_OR_LONG_2ADDR
        | OP_XOR_LONG_2ADDR
        | OP_SHL_LONG_2ADDR
        | OP_SHR_LONG_2ADDR
        | OP_USHR_LONG_2ADDR
        | OP_ADD_FLOAT_2ADDR
        | OP_SUB_FLOAT_2ADDR
        | OP_MUL_FLOAT_2ADDR
        | OP_DIV_FLOAT_2ADDR
        | OP_REM_FLOAT_2ADDR
        | OP_ADD_DOUBLE_2ADDR
        | OP_SUB_DOUBLE_2ADDR
        | OP_MUL_DOUBLE_2ADDR
        | OP_DIV_DOUBLE_2ADDR
        | OP_REM_DOUBLE_2ADDR

        | OP_IF_EQZ
        | OP_IF_NEZ
        | OP_IF_LTZ
        | OP_IF_GEZ
        | OP_IF_GTZ
        | OP_IF_LEZ

        | OP_PACKED_SWITCH
        | OP_FILL_ARRAY_DATA
        | OP_SPARSE_SWITCH

        | OP_FILLED_NEW_ARRAY
    ) operand COMMA operand
    ;

unaryInstruction:
    (
        OP_MOVE_RESULT
        | OP_MOVE_RESULT_WIDE
        | OP_MOVE_RESULT_OBJECT
        | OP_MOVE_EXCEPTION

        | OP_RETURN
        | OP_RETURN_WIDE
        | OP_RETURN_OBJECT

        | OP_MONITOR_ENTER
        | OP_MONITOR_EXIT

        | OP_THROW
        | OP_GOTO
        | OP_GOTO_16
        | OP_GOTO_32
    ) operand;

ternaryInstruction:
    (
        OP_INSTANCE_OF

        | OP_NEW_ARRAY

        | OP_IF_EQ
        | OP_IF_NE
        | OP_IF_LT
        | OP_IF_GE
        | OP_IF_GT
        | OP_IF_LE

        | OP_CMPL_FLOAT
        | OP_CMPG_FLOAT
        | OP_CMPL_DOUBLE
        | OP_CMPG_DOUBLE
        | OP_CMP_LONG

        | OP_AGET
        | OP_AGET_WIDE
        | OP_AGET_OBJECT
        | OP_AGET_BOOLEAN
        | OP_AGET_BYTE
        | OP_AGET_CHAR
        | OP_AGET_SHORT

        | OP_APUT
        | OP_APUT_WIDE
        | OP_APUT_OBJECT
        | OP_APUT_BOOLEAN
        | OP_APUT_BYTE
        | OP_APUT_CHAR
        | OP_APUT_SHORT

        | OP_IGET
        | OP_IGET_WIDE
        | OP_IGET_OBJECT
        | OP_IGET_BOOLEAN
        | OP_IGET_BYTE
        | OP_IGET_CHAR
        | OP_IGET_SHORT

        | OP_IPUT
        | OP_IPUT_WIDE
        | OP_IPUT_OBJECT
        | OP_IPUT_BOOLEAN
        | OP_IPUT_BYTE
        | OP_IPUT_CHAR
        | OP_IPUT_SHORT

        | OP_ADD_INT
        | OP_SUB_INT
        | OP_MUL_INT
        | OP_DIV_INT
        | OP_REM_INT
        | OP_AND_INT
        | OP_OR_INT
        | OP_XOR_INT
        | OP_SHL_INT
        | OP_SHR_INT
        | OP_USHR_INT
        | OP_ADD_LONG
        | OP_SUB_LONG
        | OP_MUL_LONG
        | OP_DIV_LONG
        | OP_REM_LONG
        | OP_AND_LONG
        | OP_OR_LONG
        | OP_XOR_LONG
        | OP_SHL_LONG
        | OP_SHR_LONG
        | OP_USHR_LONG
        | OP_ADD_FLOAT
        | OP_SUB_FLOAT
        | OP_MUL_FLOAT
        | OP_DIV_FLOAT
        | OP_REM_FLOAT
        | OP_ADD_DOUBLE
        | OP_SUB_DOUBLE
        | OP_MUL_DOUBLE
        | OP_DIV_DOUBLE
        | OP_REM_DOUBLE

        | OP_ADD_INT_LIT16
        | OP_RSUB_INT
        | OP_MUL_INT_LIT16
        | OP_DIV_INT_LIT16
        | OP_REM_INT_LIT16
        | OP_AND_INT_LIT16
        | OP_OR_INT_LIT16
        | OP_XOR_INT_LIT16
        | OP_ADD_INT_LIT8
        | OP_RSUB_INT_LIT8
        | OP_MUL_INT_LIT8
        | OP_DIV_INT_LIT8
        | OP_REM_INT_LIT8
        | OP_AND_INT_LIT8
        | OP_OR_INT_LIT8
        | OP_XOR_INT_LIT8
        | OP_SHL_INT_LIT8
        | OP_SHR_INT_LIT8
        | OP_USHR_INT_LIT8

        | OP_INVOKE_POLYMORPHIC
        | OP_INVOKE_POLYMORPHIC_RANGE
        | OP_INVOKE_CUSTOM
        | OP_INVOKE_CUSTOM_RANGE
        | OP_CONST_METHOD_HANDLE
        | OP_CONST_METHOD_TYPE
    ) operand COMMA operand COMMA operand
    ;

instruction:
    ternaryInstruction
    | binaryInstruction
    | unaryInstruction
    | nullaryInstruction
    ;

methodInvocationTarget:         referenceOrArrayType ARROW methodSignature;

fieldInvocationTarget:          referenceOrArrayType ARROW fieldNameAndType;

// Fields and methods

fieldName:                  identifier;

fieldType:                  anyType;

fieldNameAndType:           fieldName COLON fieldType;

fieldDirective:             FIELD_DIRECTIVE fieldModifier* fieldNameAndType (ASSIGN assignableValue)?;

classDirective:             CLASS_DIRECTIVE classModifier* referenceType;

superDirective:             SUPER_DIRECTIVE referenceType;

sourceDirective:            SOURCE_DIRECTIVE stringLiteral;

methodIdentifier:           identifier | LT identifier GT;

methodReturnType:           anyType;

methodParameterType:        (nonVoidType | methodParameterLiteral);

methodArguments:            methodParameterType+;

methodSignature:            methodIdentifier LPAREN methodArguments? RPAREN methodReturnType;

methodDeclaration:          methodModifier* methodSignature;

annotationScope:            IDENTIFIER;

annotationType:             referenceType;

annotationFieldValue:       (assignableValue | referenceType);

annotationValueScoped:      LBRACE (annotationFieldValue (COMMA annotationFieldValue)*)? RBRACE;

annotationField:            fieldName ASSIGN (annotationFieldValue|annotationValueScoped);

annotationDirective:        ANNOTATION_DIRECTIVE annotationScope annotationType annotationField* ANNOTATION_END_DIRECTIVE;

locaDirectiveVariableName:  stringLiteral;

localDirectiveType:         nonVoidType;

localDirectiveGenericHint:  stringLiteral;

localDirective:             LOCAL_DIRECTIVE registerIdentifier COMMA locaDirectiveVariableName (COLON localDirectiveType)? (COMMA localDirectiveGenericHint)?;

localEndDirective:          LOCAL_END_DIRECTIVE registerIdentifier;

localRestartDirective:      RESTART_LOCAL_DIRECTIVE registerIdentifier;

methodBodyStatement:
    registersDirective
    | localsDirective
    | paramDirective
    | lineDirective
    | instruction
    | label
    | catchDirective
    | catchAllDirective
    | annotationDirective
    | localDirective
    | localEndDirective
    | localRestartDirective
    | packedSwitchDirective
    | packedSwitchEndDirective
    | arrayDataDirective
    | sparseSwitchDirective
    ;

methodBody:                 methodBodyStatement+;

packedSwitchDirective:      PACKED_SWITCH_DIRECTIVE numericLiteral;

packedSwitchEndDirective:   PACKED_SWITCH_END_DIRECTIVE;

methodDirective:            METHOD_DIRECTIVE methodDeclaration methodBody? METHOD_END_DIRECTIVE;

registersDirective:         REGISTERS_DIRECTIVE numericLiteral;

localsDirective:            LOCALS_DIRECTIVE numericLiteral;

simpleParamDirective:       COMMA stringLiteral;

extendedParamDirective:     annotationDirective* PARAM_END_DIRECTIVE;

paramDirective:             PARAM_DIRECTIVE registerIdentifier (extendedParamDirective | simpleParamDirective);

lineDirective:              LINE_DIRECTIVE numericLiteral;

fromLabel:                  label;

toLabel:                    label;

catchDirective:             CATCH_DIRECTIVE referenceType LBRACE fromLabel DOT DOT toLabel RBRACE label;

catchAllDirective:          CATCHALL_DIRECTIVE LBRACE fromLabel DOT DOT toLabel RBRACE label;

arrayDataDirective:         ARRAY_DATA_DIRECTIVE numericLiteral arrayDataEntry* ARRAY_DATA_END_DIRECTIVE;

arrayDataEntry:             numericLiteral IDENTIFIER?;

sparseSwitchDirectiveValue: numericLiteral ARROW label;

sparseSwitchDirective:      SPARSE_SWITCH_DIRECTIVE sparseSwitchDirectiveValue* SPARSE_SWITCH_END_DIRECTIVE;

statement:
    classDirective
    | superDirective
    | sourceDirective
    | fieldDirective
    | methodDirective
    ;

parse:
    statement+
    ;
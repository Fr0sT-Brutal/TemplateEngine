// // DVDChi Template Engine Version 1.0.0 for Delphi XE/XE2/XE3
//
// SOFTWARE CAN NOT BE USED IN ANY HOME INVENTORY OR CATALOGING SOFTWARE
// (FREEWARE OR SHAREWARE) WITHOUT OUR WRITTEN PERMISSION
//
// The contents of this file are subject to the GNU General Public License
// You may redistribute this library, use and/or modify it under the terms of the
// GNU Lesser General Public License as published by the Free Software Foundation;
// either version 2.1 of the License, or (at your option) any later version.
// You may obtain a copy of the GPL at http://www.gnu.org/copyleft/.
//
// Alternatively, you may redistribute this library, use and/or modify it under the terms of the
// Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The initial developer of the original code is Adit Software
// written by Denis Sletkov (dvd@dvdchief.com, dvdchief.com/delphi).

unit TemplateEngine;

{$J-}    //Writeable typed constants off
{$H+}    //Long strings used
{$P+}    //Open String Parameters used
{$T-}    //Generate untyped pointer
{$X+}    //Extended syntax on
{$B-}    //Boolean short-circuit evaluation
{$O+}    //Optimization on
{$R-}    //Range checking off
{$IFDEF DCC}
  {$HIGHCHARUNICODE ON} // Delphi: consider all string literals WideChar
{$ENDIF}

{.$DEFINE SMARTYDEBUG}
{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {VCL}
  Classes, SysUtils, StrUtils, DateUtils, Variants,
  Character, Generics.Collections;

type
  ESmartyException = class (Exception)
  end;

  TTemplateAction = class;
  TForEachOutputAction = class;
  TOperation = class;
  TSmartyEngine = class;

  //record for DateLoose format
  TDateRecord = packed record
    Year: Word;         //0 means undefine
    Month, Day: Byte;
    function GetVariant: Variant;
    procedure SetVariant(AValue: Variant);
  end;

  TVariableType = (vtNull,               //Not defined variable, error in evaluation
                   vtBoolean,
                   vtInt,                //Int64 variable
                   vtFloat,              //Float variable
                   vtDateStrict,         //DateStrict variable TDate
                   vtDateLoose,          //DateLoose variable for fuzzy date
                   vtDateTime,           //DateTime variable
                   vtString,             //string variable
                   vtArray               //Associate Array
                   );
(*
  COMPARE MATRIX

               NULL    BOOL    INT    FLOAT    DATE    DLOOSE    DTIME     STRING   ARRAY
  NULL == FALSE BOOL   BOOL    BOOL   BOOL     BOOL     BOOL      BOOL      BOOL     BOOL
  BOOL          *      BOOL    BOOL   BOOL     BOOL     BOOL      BOOL      BOOL     BOOL
  INT           *       *      INT    FLOAT    INT     DLOOSE    FLOAT      FLOAT
  FLOAT         *       *       *     FLOAT    FLOAT   DLOOSE    FLOAT      FLOAT
  DATE          *       *       *      *       DATE    DLOOSE    DTIME      DATE
  DLOOSE        *       *       *      *        *      DLOOSE    DLOOSE    DLOOSE
  DTIME         *       *       *      *        *         *      DTIME      DTIME
  STRING        *       *       *      *        *         *        *       STRING
  ARRAY         *       *       *      *        *         *        *         *


*)


  TVariableTypes = set of TVariableType;

  TCompareOperation = (coEq, coNeq, coGt, coLt, coGte, coLte, coSEq);

  TBinaryOperation = (voAdd, voSubtract, voMultiply,     //IntFloatOps
                      voDivide,                          //FloatOps
                      voIntDivide, voModulus,            //IntOps
                      voShl, voShr,
                      voAnd, voOr, voXor                 //LogicalIntOps
                      );

  TVariableRelatioship = (vrGreaterThan, vrEqual, vrLessThan);

  //variable record for store variable

  PVariableRecord = ^TVariableRecord;
  TVariableRecord = packed record
  public
    VarType: TVariableType;

    procedure Finalize;
    function Clone: TVariableRecord;
    function IsNull: Boolean;
    function IsEmpty: Boolean;
    function IsArray: Boolean;

    function IsBoolean: Boolean;
    function IsInt: Boolean;
    function IsFloat: Boolean;
    function IsNumber: Boolean;
    function IsDateStrict: Boolean;
    function IsDateLoose: Boolean;
    function IsDateTime: Boolean;
    function IsDate: Boolean;
    function IsString: Boolean;

    class function Null: TVariableRecord; static;
    class function AsInt(AValue: Int64; ANullValue: Int64 = 0): TVariableRecord; static;
    class function AsFloat(AValue: Double; ANullValue: Double = 0): TVariableRecord; static;
    class function AsString(const AValue: string; const ANullValue: string = ''): TVariableRecord; static;
    class function AsDateRecord(const AValue: TDateRecord): TVariableRecord; static;
    procedure SetNull;
    procedure SetBool(AValue: Boolean);
    procedure SetInt(AValue: Int64);
    procedure SetFloat(Avalue: Double);
    procedure SetString(const AValue: string);
    procedure SetArrayLength(AValue: Integer; AReference: TObject = nil; AInit: Boolean = False);
    procedure SetArrayItem(AIndex: Integer; const AKey: string; const AValue: TVariableRecord);
    procedure SetArrayItemQ(AIndex: Integer; const AKey: string; const AValue: TVariableRecord);

    function ToBool: Boolean;
    function ToInt: Int64;
    function ToFloat: Double;
    function ToString: string;
    function CanConvertToLogical(out Value: Boolean): Boolean;
    function CanConvertToInt(out Value: Int64): Boolean;
    function CanConvertToFloat(out Value: Double): Boolean;
    class function DoCompareRelationship(const ALeft, ARight: TVariableRecord;
      AOperation: TCompareOperation): TVariableRelatioship; static;
    class function DoCompare(const ALeft, ARight: TVariableRecord;
      AOperation: TCompareOperation): Boolean; static;
    class function DoIntFloatOp(const ALeft, ARight: TVariableRecord;
      AOperation: TBinaryOperation): TVariableRecord; static;
    class function DoFloatOp(const ALeft, ARight: TVariableRecord;
      AOperation: TBinaryOperation): TVariableRecord; static;
    class function DoIntOp(const ALeft, ARight: TVariableRecord;
      AOperation: TBinaryOperation): TVariableRecord; static;
    class function DoIntNot(const ARight: TVariableRecord): TVariableRecord; static;
    class function DoLogicalOp(const ALeft, ARight: TVariableRecord;
      AOperation: TBinaryOperation): TVariableRecord; static;
    class function DoLogicalNot(const ARight: TVariableRecord): TVariableRecord; static;

    class operator Implicit(AValue: Boolean): TVariableRecord;
    class operator Implicit(AValue: Integer): TVariableRecord;
    class operator Implicit(AValue: Double): TVariableRecord;
    class operator Implicit(AValue: Extended): TVariableRecord;
    class operator Implicit(AValue: TDate): TVariableRecord;
    class operator Implicit(const AValue: TDateRecord): TVariableRecord;
    class operator Implicit(AValue: TDateTime): TVariableRecord;
    class operator Implicit(const AValue: string): TVariableRecord;
    class operator Implicit(AValue: Cardinal): TVariableRecord;
    class operator Implicit(AValue: Int64): TVariableRecord;
    class operator Implicit(AValue: UInt64): TVariableRecord;
    class operator Implicit(const AValue: Variant): TVariableRecord;

    class operator Implicit(const ARecord: TVariableRecord): Boolean;
    class operator Implicit(const ARecord: TVariableRecord): Integer;
    class operator Implicit(const ARecord: TVariableRecord): Double;
    class operator Implicit(const ARecord: TVariableRecord): string;

    class operator Add(const ALeft, ARight: TVariableRecord): TVariableRecord;
    class operator Subtract(const ALeft, ARight: TVariableRecord): TVariableRecord;
    class operator Multiply(const ALeft, ARight: TVariableRecord): TVariableRecord;
    class operator Divide(const ALeft, ARight: TVariableRecord): TVariableRecord;
    class operator IntDivide(const ALeft, ARight: TVariableRecord): TVariableRecord;
    class operator Modulus(const ALeft, ARight: TVariableRecord): TVariableRecord;
    class operator LeftShift(const ALeft, ARight: TVariableRecord): TVariableRecord;
    class operator RightShift(const ALeft, ARight: TVariableRecord): TVariableRecord;
    class operator LogicalAnd(const ALeft, ARight: TVariableRecord): TVariableRecord;
    class operator LogicalOr(const ALeft, ARight: TVariableRecord): TVariableRecord;
    class operator LogicalXor(const ALeft, ARight: TVariableRecord): TVariableRecord;

    case TVariableType of
      vtNull:        ();
      vtBoolean:     (BValue: Boolean);
      vtInt:         (IValue: Int64);
      vtFloat:       (FValue: Double);
      vtDateStrict:  (DSValue: TDate);
      vtDateLoose:   (DLValue: TDateRecord);
      vtDateTime:    (DTValue: TDateTime);
      vtString:      (SValue: Pointer);
      vtArray:       (AValue: Pointer);  //PVariableArray
  end;

  TVariableArrayItem = packed record
    Key: string;
    Item: TVariableRecord;
  end;

  PVariableArrayData = ^TVariableArrayData;
  TVariableArrayData = array [0..0] of TVariableArrayItem;

  PVariableArray = ^TVariableArray;
  TVariableArray = packed record
    Count: Integer;
    Reference: TObject;
    Data: PVariableArrayData;
  end;

  //Variable structure

  TVariablePartType = (vptValue, vptIndex);

  TVariablePart = record
    PartType: TVariablePartType;

    procedure Finalize;
    function Clone: TVariablePart;
    class operator Implicit(AValue: Int64): TVariablePart;
    class operator Implicit(const AValue: string): TVariablePart;
    class operator Implicit(const APart: TVariablePart): Integer;
    class operator Implicit(const APart: TVariablePart): string;

    case TVariablePartType of
      vptValue: (SValue: Pointer);
      vptIndex: (IValue: Int64);
  end;

  TVarList = class (TList<TVariablePart>)
  public
    function Clone: TVarList;
    procedure Finalize;
    procedure DeleteElement(Index: Integer);
    procedure AddArrayPrefix(AVariable: TVarList; Index: Integer);
    function IsSimpleVariable(out VarName: string): Boolean;
    function CheckTopLevel(const AName: string): Boolean;
    function IsTopValueLevel(out AName: string): Boolean;
  end;

  //Register Namespaces:
  // smarty - system variables

  TNamespaceProvider = class
  strict private
    FName: string;
    FIsIndexSupported: Boolean;
    FUseCache: Boolean;
    FMin, FMax: Integer;
  public
    constructor Create(const AName: string; UseCache: Boolean = False;
                       IsIndexSupported: Boolean = False;
                       Min: Integer = 0; Max: Integer = 0); overload;
    function GetName: string; virtual;      //Get Namespace Name
    function IsIndexSupported: Boolean; virtual;
    function UseCache: Boolean; virtual;
    procedure GetIndexProperties(var AMin, AMax: Integer); virtual;
    function GetVariable(AIndex: Integer;
      const AVarName: string): TVariableRecord; virtual; abstract;
  end;

  TStorageNamespaceProvider = class(TNamespaceProvider)
  strict private
    FVariables: TDictionary<string,TVariableRecord>;
    procedure InternalSetVariable(const AVarName: string; const Value: TVariableRecord); overload;
  protected
    function InternalGetVariable(const AVarName: string): TVariableRecord;
  public
    destructor Destroy; override;

    function GetVariable(AIndex: Integer;
      const AVarName: string): TVariableRecord; override;

    procedure ClearVariables;
    procedure SetVariable(const AVarName: string; const Value: TVariableRecord); overload;
    procedure SetVariable(const AVarName: string; const Values: array of TVariableRecord); overload;
    procedure SetVariables(const Variables: array of TVariableArrayItem);
  end;

  TForEachData = class
    Name: string;
    InForEach: Boolean;
    ItemVarName, KeyVarName: string;
    Iteration: Integer;   //from 1 to Count
    First: Boolean;
    Last: Boolean;
    Show: Boolean;
    Total: Integer;
    IsNamespace: Boolean;
    Namespace: TNamespaceProvider;
    MinIndex: Integer;
    VarData: PVariableArray;
  end;

  TForEachList = class (TList<TForEachData>)
  private
    CurrentRecords: TList<Integer>;
    procedure EnterForEach(AList: TForEachData);
    procedure ExitForEach;
    function InForEach: Boolean;
    function FindItemRecord(const AItemName: string; out ARecord: TForEachData): Boolean;
    function FindKeyRecord(const AKeyName: string; out ARecord: TForEachData): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function FindRecord(const AName: string; out ARecord: TForEachData): Boolean;
  end;

  TCaptureCache = record
    VariableName: string;            //variable name
    VariableValue: PVariableRecord;  //variable value
  end;

  TCaptureArrayItem = class
  private
    IsActive: Boolean;
    ItemName: string;
    Index: Integer;
    VarData: PVariableArray;
    procedure Enter(const AName: string; AIndex: Integer; AVarData: PVariableArray);
    procedure IncIndex;
    procedure Exit;
    function IsItemName(const AName: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  //smarty provider
  TSmartyProvider = class (TNamespaceProvider)
  private
    FEngine: TSmartyEngine;
    FCaptureCache: TList<TCaptureCache>;
    FForEachList: TForEachList;
    FActiveCapture: TCaptureArrayItem;
    procedure ClearCaptureCache;
    function FindCaptureItem(const AName: string; var Cache: TCaptureCache): Boolean;
    procedure SetCaptureItem(const AName: string; const VariableValue: TVariableRecord);
    procedure RemoveCaptureItem(const AName: string);
  public
    constructor Create(AEngine: TSmartyEngine);
    destructor Destroy; override;
    function GetName: string; override;     //Get Namespace Name
    function IsIndexSupported: Boolean; override;
    function UseCache: Boolean; override;
    procedure GetIndexProperties(var AMin, AMax: Integer); override;
    function GetVariable(AIndex: Integer; const AVarName: string): TVariableRecord; override;
    function GetSmartyVariable(const AVarName: string; AVarDetails: TVarList;
      var NeedFinalize: Boolean): TVariableRecord;
    function GetDetachVariable(const AVarName: string; AVarDetails: TVarList;
      var NeedFinalize: Boolean): TVariableRecord;
  end;

  //Variable modifier

  TVariableModifierClass = class of TVariableModifier;

  TVariableModifier = class
  protected
    class function CheckParams(AModifier: TVariableModifierClass;
      AParams: TStringList;  AMin, AMax: Integer): Boolean;
    class function SetParam(AParams: TStringList; AIndex: Integer;
      var Value: string): Boolean;
  public
    class function GetName: string; virtual; abstract;
    class function CheckInputParams(AParams: TStringList): Boolean; virtual; abstract;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); virtual; abstract;
    class procedure ModifyVariable(const AVariable: TVariableRecord;
      AParams: TStringList); virtual;
  end;

  TCapitalizeModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  TCatModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  TTrimModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  TCountCharactersModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  TCountParagraphsModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  TCountWordsModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  TDefaultModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  THTMLEncodeModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  THTMLEncodeAllModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  TXMLEncodeModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  TFileEncodeModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  TDateFormatModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  TFloatFormatModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  TLowerModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  TUpperModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  TNl2BrModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  TTruncateModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  TStripModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  TSpacifyModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  TWordwrapModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  TIndentModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  TReplaceModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;

  TStripTagsModifier = class (TVariableModifier)
    class function GetName: string; override;
    class function CheckInputParams(AParams: TStringList): Boolean; override;
    class procedure ModVariable(const AVariable: TVariableRecord;
      AParams: TStringList); override;
  end;


  //Functions

  TSmartyFunctionClass = class of TSmartyFunction;

  TSmartyFunction = class
  protected
    class function IsParam(Index: Integer; const AParams: array of TVariableRecord;
      var Param: TVariableRecord): Boolean;
    class function GetParam(Index: Integer; const AParams: array of TVariableRecord): TVariableRecord;
  public
    class function GetName: string; virtual; abstract;
    class function CheckParams(AParamsCount: Integer): Boolean; virtual; abstract;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; virtual; abstract;
    class function EvaluateFunction(const AParams: array of TVariableRecord): TVariableRecord; virtual;
  end;

  //Is... functions

  TIsNullFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TIsEmptyFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TIsBooleanFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TIsIntegerFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TIsFloatFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TIsNumberFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TIsDateStrictFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TIsDateLooseFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TIsDateTimeFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TIsDateFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TIsStringFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TIsArrayFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TArrayLengthFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TArrayIndexFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TArrayKeyFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TCountFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  //String Functions

  TEchoFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TPrintFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  THTMLEncodeFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  THTMLEncodeAllFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TXMLEncodeFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TFileEncodeFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TTrimFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TTruncateFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TStripFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TStripTagsFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TSpacifyFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TWordwrapFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TIndentFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TCapitalizeFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TCountCharactersFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TCountWordsFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TCountParagraphsFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TUpperCaseFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TLowerCaseFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TResemblesFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TContainsFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TStartsFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TEndsFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TReplaceFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  //string float_format(float $float, string $format = "")
  TFloatFormatFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  //mixed ifthen(bool $condition, mixed $tcond, mixed $fcond)
  TIfThenFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  //DateTime Functions

  TDateFormatFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TFullYearsFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TYearOfFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TMonthOfFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;

  TDayOfFunction = class (TSmartyFunction)
    class function GetName: string; override;
    class function CheckParams(AParamsCount: Integer): Boolean; override;
    class function Evaluate(const AParams: array of TVariableRecord): TVariableRecord; override;
  end;


  TTemplateActionType = (tatRawOutput, tatVariableOutput, tatFuncOutput,
    tatIf, tatForEach, tatCaptureArray, tatReleaseArray, tatAssign, tatRelease);

  TNestAction = (naNone, naIf, naForEach);

  TTemplateActions = class (TObjectList<TTemplateAction>)
    function Execute: string;
  end;

  TTemplateAction = class
  private
    FEngine: TSmartyEngine;
    FActionType: TTemplateActionType;
  public
    property ActionType: TTemplateActionType read FActionType;

    constructor Create(AEngine: TSmartyEngine); virtual;
    function Execute: string; virtual; abstract;
    class function IsComment(var ACommand: string): Boolean;
    class function IsTag(const ATag: string; const ACommand: string;
       AOnlyTag: Boolean = False): Boolean;
    class function IsTagAndGetCommand(const ATag: string;
      var ACommand: string): Boolean;
    class function IsExitCommand(const ACommand: string;
      ABreakAction: TNestAction): Boolean;
    class function ParseFunction(const ACommand: string): TStringList;
    class procedure CheckFunction(ACommand: TStringList;
      const AValid: array of string);
    class function GetAttributeValue(ACommand: TStringList; const AAtribute: string;
      const ADefault: string = ''): string;
    class procedure ExtractFunctionItem(ACommand: TStringList; Index: Integer;
      var Name, Value: string);
    class procedure ParseVariable(const AVariable: string; AVarList: TVarList);
    class procedure GetVariableProperties(AEngine: TSmartyEngine;
      const AVariable: string; var Namespace: TNamespaceProvider;
      var Index: Integer; var VarName: string; var AVarList: TVarList);
    class function IsAction(AEngine: TSmartyEngine; const ACommand: string;
      var AAction: TTemplateAction): Boolean; virtual;
  end;

  TRawOutputAction = class (TTemplateAction)
  strict private
    FOutput: string;
  public
    property Output: string read FOutput;

    constructor Create(AEngine: TSmartyEngine); override;
    constructor CreateOutput(AEngine: TSmartyEngine; const AOutput: string);
    function Execute: string; override;
    class function IsAction(AEngine: TSmartyEngine;  const ACommand: string;
      var AAction: TTemplateAction): Boolean; override;
  end;

  TModifierAction = class
  private
    FModifier: TVariableModifierClass;
    FParams: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TVariableOutputAction = class (TTemplateAction)
  strict private
    FNamespace: TNamespaceProvider;
    FIndex: Integer;
    FVarName: string;
    FVarDetails: TVarList;
    FModifiers: TObjectList<TModifierAction>;
    procedure SetVariable(AEngine: TSmartyEngine; const AVariable: string);
  public
    constructor Create(AEngine: TSmartyEngine); override;
    destructor Destroy; override;
    function Execute: string; override;
    class function IsAction(AEngine: TSmartyEngine;
      const ACommand: string; var AAction: TTemplateAction): Boolean; override;
  end;

  TFuncOutputAction = class (TTemplateAction)
  private
    FOperation: TOperation;
    FModifiers: TObjectList<TModifierAction>;
  public
    constructor Create(AEngine: TSmartyEngine); override;
    destructor Destroy; override;
    function Execute: string; override;
    class function IsAction(AEngine: TSmartyEngine;
      const ACommand: string; var AAction: TTemplateAction): Boolean; override;
  end;

  TOperator = (opEq,      //eq, =
               opNeq,     //ne, neq, !=, <>
               opGt,      //gt, >
               opLt,      //lt, <
               opGte,     //gte, ge, >=
               opLte,     //lte, le, <=
               opSEq,     //seq, ==

               opAdd,     //+
               opSub,     //-
               opMply,    //*
               opDivide,  // /
               opMod,     //mod, %
               opDiv,     //div, \
               opShl,     //shl, <<
               opShr,     //shr, >>

               opLogicalNot,     //not, !
               opLogicalAnd,     //and, &&
               opLogicalOr,      //or, ||

               opBitwiseNot,     //~
               opBitwiseAnd,     //bitand, &
               opBitwiseOr,      //bitor, |
               opBitwiseXor      //xor, ^
               );

  TOperation = class
    constructor Create; virtual;
    function Evaluate(AEngine: TSmartyEngine; var NeedFinalize: Boolean): TVariableRecord; virtual; abstract;
    class function Parse(AEngine: TSmartyEngine; const S: string): TOperation;
    {$IFDEF SMARTYDEBUG} function AsString: string; virtual; abstract; {$ENDIF}
  end;

  TOperationList = class (TObjectList<TOperation>)
  end;

  TOpVariable = class (TOperation)
  private
    FNamespace: TNamespaceProvider;
    FIndex: Integer;
    FVarName: string;
    FVarDetails: TVarList;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Evaluate(AEngine: TSmartyEngine;
      var NeedFinalize: Boolean): TVariableRecord; override;
    {$IFDEF SMARTYDEBUG} function AsString: string; override; {$ENDIF}
  end;

  TOpConst = class (TOperation)
  private
    FValue: TVariableRecord;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Evaluate(AEngine: TSmartyEngine;
      var NeedFinalize: Boolean): TVariableRecord; override;
    {$IFDEF SMARTYDEBUG} function AsString: string; override; {$ENDIF}
  end;

  TOpFunction = class (TOperation)
  private
    FFuncClass: TSmartyFunctionClass;
    FParams: TOperationList;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Evaluate(AEngine: TSmartyEngine;
      var NeedFinalize: Boolean): TVariableRecord; override;
    {$IFDEF SMARTYDEBUG} function AsString: string; override; {$ENDIF}
  end;

  TOpOperator = class (TOperation)
  private
    FOperator: TOperator;
    FLeftOp, FRightOp: TOperation;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Evaluate(AEngine: TSmartyEngine;
      var NeedFinalize: Boolean): TVariableRecord; override;
    {$IFDEF SMARTYDEBUG} function AsString: string; override; {$ENDIF}
  end;


  TIfType = (ifSimple, ifDef, ifNDef, ifEmpty, ifNEmpty);

  TIfCondition = class
  private
    FEngine: TSmartyEngine;
    FIfType: TIfType;
  public
    property IfType: TIfType read FIfType;
    function Evaluate: Boolean; virtual; abstract;
    constructor Create(AEngine: TSmartyEngine); virtual;
  end;

  TSimpleIf = class (TIfCondition)
  private
    FOperation: TOperation;
  public
    constructor Create(AEngine: TSmartyEngine); override;
    constructor CreateOperation(AEngine: TSmartyEngine; AExpr: string);
    destructor Destroy; override;
     function Evaluate: Boolean; override;
  end;

  TVariableIf = class (TIfCondition)
  private
    FNamespace: TNamespaceProvider;
    FIndex: Integer;
    FVarName: string;
    FVarDetails: TVarList;
    procedure SetVariable(AEngine: TSmartyEngine; const AVariable: string);
  public
    constructor Create(AEngine: TSmartyEngine); override;
    constructor CreateIf(AEngine: TSmartyEngine; AType: TIfType);
    destructor Destroy; override;
    function Evaluate: Boolean; override;
  end;

  TElseIfAction = class
  private
    FCondition: TIfCondition;
    FActions: TTemplateActions;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TElseIfActions = class(TObjectList<TElseIfAction>)
  end;

  TIfOutputAction = class (TTemplateAction)
  private
    FCondition: TIfCondition;
    FThenActions, FElseActions: TTemplateActions;
    FElseIfActions: TElseIfActions;
    function ContinueIf(AEngine: TSmartyEngine; const ACommand: string;
      var AState: Integer; var AActions: TTemplateActions): Boolean;
  public
    property ThenActions: TTemplateActions read FThenActions;
    property ElseActions: TTemplateActions read FElseActions;
    property ElseIfActions: TElseIfActions read FElseIfActions;

    constructor Create(AEngine: TSmartyEngine); override;
    destructor Destroy; override;
    function Execute: string; override;
    class function IsAction(AEngine: TSmartyEngine;  const ACommand: string;
      var AAction: TTemplateAction): Boolean; override;
  end;

  TForEachOutputAction = class (TTemplateAction)
  private
    FNamespaceBased: Boolean;
    FNamespace: TNamespaceProvider;
    FIndex: Integer;
    FVarName: string;
    FVarDetails: TVarList;
    FForEachName: string;
    FVariableName: string;
    FAssocName: string;
    FMaxItems: Integer;

    FBaseActions: TTemplateActions;
    FElseActions: TTemplateActions;
    function ContinueForEach(AEngine: TSmartyEngine; const ACommand: string;
      var AState: Integer; var AActions: TTemplateActions): Boolean;
  public
    property NamespaceBased: Boolean read FNamespaceBased;
    property VarDetails: TVarList read FVarDetails;
    property ForEachName: string read FForEachName;
    property VariableName: string read FVariableName;
    property AssocName: string read FAssocName;
    property BaseActions: TTemplateActions read FBaseActions;
    property ElseActions: TTemplateActions read FElseActions;

    constructor Create(AEngine: TSmartyEngine); override;
    destructor Destroy; override;
    function Execute: string; override;
    class function IsAction(AEngine: TSmartyEngine; const ACommand: string;
      var AAction: TTemplateAction): Boolean; override;
  end;

  TCaptureArrayAction = class (TTemplateAction)
  private
    FNamespace: TNamespaceProvider;
    FIndex: Integer;
    FVarName: string;
    FVarDetails: TVarList;
    FItemName: string;
    FVariableName: string;
    FFilter: TOperation;
  public
    property VarDetails: TVarList read FVarDetails;

    constructor Create(AEngine: TSmartyEngine); override;
    destructor Destroy; override;
    function Execute: string; override;
    class function IsAction(AEngine: TSmartyEngine; const ACommand: string;
      var AAction: TTemplateAction): Boolean; override;
  end;

  TReleaseArrayAction = class (TTemplateAction)
  private
    FVariableName: string;
  public
    constructor Create(AEngine: TSmartyEngine); override;
    destructor Destroy; override;
    function Execute: string; override;
    class function IsAction(AEngine: TSmartyEngine; const ACommand: string;
      var AAction: TTemplateAction): Boolean; override;
  end;

  TAssignAction = class (TTemplateAction)
  private
    FVariableName: string;
    FValue: TOperation;
  public
    constructor Create(AEngine: TSmartyEngine); override;
    destructor Destroy; override;
    function Execute: string; override;
    class function IsAction(AEngine: TSmartyEngine; const ACommand: string;
      var AAction: TTemplateAction): Boolean; override;
  end;

  TReleaseAction = class (TTemplateAction)
  private
    FVariableName: string;
  public
    constructor Create(AEngine: TSmartyEngine); override;
    destructor Destroy; override;
    function Execute: string; override;
    class function IsAction(AEngine: TSmartyEngine; const ACommand: string;
      var AAction: TTemplateAction): Boolean; override;
  end;

  TVariableCache = record
    Namespace: TNamespaceProvider;   //nil, if not namespace
    Index: Integer;                  //-1, if no index
    VariableName: string;            //variable name
    VariableValue: PVariableRecord;  //variable value
  end;

  TSmartyInfoProvider = class
  private
    //modifiers
    FModifiers: TStringList;

    //function
    FFunctions: TStringList;

    procedure Init;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddModifier(AModifier: TVariableModifierClass);
    procedure AddFunction(AFunction: TSmartyFunctionClass);
    procedure DeleteFunction(AFunction: TSmartyFunctionClass);
  end;

  TSmartyEngine = class
  strict private
    FCompiled: Boolean;
    FActions: TTemplateActions;

    //namespaces
    FNamespaces: TStringList;
    FSmartyNamespace: TSmartyProvider;

    FIsCache: Boolean;
    FVarCache: TList<TVariableCache>;

    //error handling
    FSilentMode: Boolean;
    FErrors: TStringList;
    FAutoHTMLEncode: Boolean;
    FAllowEspacesInStrings: Boolean;
    FStripLineBreaksAfterBlocks: Boolean;

    //template properties
    FTemplateFolder: string;

    procedure Init;
    procedure SetIsCache(Value: Boolean);
  private // for internal use
    function GetVariable(const ANamespace: TNamespaceProvider; AIndex: Integer;
      const AVariableName: string; ADetails: TVarList; var NeedFinalize: Boolean): TVariableRecord;
    function GetVariableDetails(const AVariable: TVariableRecord; ADetails: TVarList): TVariableRecord;
    class function IsFunction(const ACommand: string; var Func: TSmartyFunctionClass;
      var Params: string; var Modifiers: string): Boolean;
    class function GetFunction(const AFunction: string): TSmartyFunctionClass;
    property Namespaces: TStringList read FNamespaces;
    property SmartyNamespace: TSmartyProvider read FSmartyNamespace;
  public
    constructor Create;
    destructor Destroy; override;
    function Compile(const ADocument: string; var Errors: TStringList): Boolean;
    function Execute: string; overload;
    function Execute(const Pattern: string; out Errors, Output: string): Boolean; overload;
    procedure ClearCache; overload;
    procedure ClearCache(ANamespace: TNamespaceProvider); overload;

    procedure AddNamespace(ANamespace: TNamespaceProvider);
    procedure DeleteNamespace(ANamespace: TNamespaceProvider);

    property Compiled: Boolean read FCompiled;
    property SilentMode: Boolean read FSilentMode write FSilentMode;
    property Errors: TStringList read FErrors;
    property AutoHTMLEncode: Boolean read FAutoHTMLEncode write FAutoHTMLEncode;
    property AllowEspacesInStrings: Boolean read FAllowEspacesInStrings write FAllowEspacesInStrings;
    property StripLineBreaksAfterBlocks: Boolean read FStripLineBreaksAfterBlocks write FStripLineBreaksAfterBlocks;
    property IsCache: Boolean read FIsCache write SetIsCache;
    property TemplateFolder: string read FTemplateFolder write FTemplateFolder;
  end;

(* Smarty template engine syntax

  Variable syntax
  $namespace.variablename[0].otherpart

  {literal} ... {/literal} - skip part of Templates as is
  {ldelim} - symbol {
  {rdelim} - symbol }

  {* comment *}            - comment in template
  { $namespace.variable | modifier:"param1":"param2" | modifier } - put variable


  { ifdef $namespace.var }
  { ifndef $namespace.var }
  { ifempty $namespace.var }
  { ifnempry $namespace.var }

  { else }
  { elseifdef $namespace.var }
  { elseifndef $namespace.var }
  { elseifempty $namespace.var }
  { elseifnempry $namespace.var }

  {/if}

  {foreach

*)

//convertion rountines

// Helper routine to construct value record from an array of values.
// Items are added with empty key.
function Arr(const Values: array of TVariableRecord): TVariableRecord;
// Helper routine to construct value record from an array of name-value pairs
function Map(const Items: array of TVariableArrayItem): TVariableRecord;
// Helper routine to construct name-value pair record from a single value
// JSON analogy: 'foo', 10 => {'foo': 10}
function Item(const Key: string; const Value: TVariableRecord): TVariableArrayItem; overload;
// Helper routine to construct name-value pair record from an array of values.
// Items are added with empty key.
// JSON analogy: 'foo', [10, true, "str"] => {'foo': [10, true, "str"]}
//   FArrOfItems[N] := Item('arr', ['test string', 29, 2.0, true]);
//   SmartyExec(Template, 'test', FArrOfItems, Errors, Actual)
function Item(const Key: string; const Values: array of TVariableRecord): TVariableArrayItem; overload;
// Helper routine to construct name-value pair record from an array of name-value pairs
// JSON analogy: 'foo', [{'bar': 1}, {'quz': 2}] => {'foo': [{'bar': 1}, {'quz': 2}]}
//   FArrOfItems[N] := Item('map',
//     [
//       Item('field1', 123),
//       Item('field2', 'foo')
//     ]);
//   SmartyExec(Template, 'test', FArrOfItems, Errors, Actual)
function Item(const Key: string; Items: array of TVariableArrayItem): TVariableArrayItem; overload;

function DateRecordToStr(var Value: TDateRecord): string;         //use FormatSettings
function DateRecordToString(const Value: TDateRecord): string;      //FormatSettings independent
function StringToDateRecord(const Value: string): TDateRecord;
function DateTimeFromRecord(const Value: TDateRecord): TDateTime;
function DateTimeToRecord(Value: TDateTime): TDateRecord;
function IsEmpty(const Value: TDateRecord): Boolean;
function GetDateRecordVariant(AYear: Word; AMonth: Word = 0; ADay: Word = 0): Variant;

function GetStartDate(const Value: TDateRecord): TDateTime;
function GetEndDate(const Value: TDateRecord): TDateTime;

function DoValidIdent(const Value: string): string;

function ParseEscapes(const AStr: string): string;
function SmartyTrim(const S: string): string;
function SmartyTrimLeft(const S: string): string;
function SmartyTrimRight(const S: string): string;
function UCWords(const AStr: string; ACapitalizeDigits: Boolean = False): string;
function CountCharacters(const AStr: string; ACountWhitespace: Boolean = False): Integer;
function TruncateString(const AStr: string; ALength: Integer = 80; AEtc: string = '...';
  ABreakWords: Boolean = False; AMiddle: Boolean = False): string;
function Strip(const AStr: string; AStripStr: string = ' '): string;
function StripTags(const AStr: string; ANoSpace: Boolean = True;
  AParse: Boolean = True): string;
function Spacify(const AStr: string; ASpacifyStr: string = ' '): string;
function Wordwrap(const Line: string; MaxCol: Integer = 80;
  const BreakStr: string = sLineBreak): string;
function IsLineBreak(C: Char): Boolean; inline;
function CountParagraphs(const AStr: string): Integer;
function CountWords(const AStr: string): Integer;
function IndentString(const AStr: string; IndentStr: string = ' '): string;

function XMLEncode(const AStr: string): string;
function HTMLEncode(const AStr: string): string;
function HTMLEncodeEntities(const AStr: string): string;
function FileEncode(const S: string): string;

//register function or modifier
procedure RegisterModifier(AModifier: TVariableModifierClass);
procedure RegisterFunction(AFunction: TSmartyFunctionClass);
procedure UnregisterFunction(AFunction: TSmartyFunctionClass);
function SmartyExec(const Pattern, NamespaceName: string; const Variables: array of TVariableArrayItem;
                    out Errors, Output: string): Boolean;

function GetFileContent(const AFilename: string; AEncoding: TEncoding): string;

resourcestring
  sIncorrectArrayItem = 'Invalid key value or array index';
  sIncorrectArrayKey = 'Invalid key value';
  sInvalidParameters = 'Invalid parameters count (%d), required %d-%d in modified %s';
  sInvalidIfCommand = 'Command "%s" found outside of if block';
  sInvalidForEachCommand = 'Command "%s" found outside of foreach block';
  sDuplicateAttribute = 'Duplicate attribute name "%s"';
  sInvalidAttribute = 'Invalid attribute "%s"';
  sInvalidArrayIndex = 'Array index expected by "[" symbol found in variable "%s"';
  sUnpairBrackets = 'Unpair brackets "[" found in variable "%s"';
  sInvalidCharsInArrayIndex = 'Invalid char in variable "%s" array index';
  sInvalidVarChars = 'Invalid char "%s" in variable "%s"';
  sUnclosedBrackets = 'Unclose brackets "[" found in variable "%s"';
  sNamespaceIndexMiss = 'There is no namespace index declaration in variable "%s"';
  sNamespaceVarMiss = 'There is no namespace variable name declaration in variable "%s"';
  sInvalidVariable = 'Invalid variable "%s" declaration';
  sInvalidModifierParams = 'Invalid parameter or modifier name "%s"';
  sInvalidModifier = 'Modifier "%s" do not found';
  sInvalidTemplateChar = 'Unexpected symbol "%s" in template command "%s"';
  sInvalidFunction = 'Function "%s" do not found.';
  sInvalidIntegerConst = 'Invalid integer constant "%s" declaration';
  sInvalidFloatConst = 'Invalid float constant "%s" declaration';
  sInvalidDateConst = 'Invalid date constant "%s" declaration';
  sUncloseFunctionDeclaration = 'Function declaration "%s" is unclosed';
  sFunctionParamsMiss = 'Function "%s" parameters is missed';
  sPathensisDoNotClosed = 'Pathensis ")" do not have corresponding ("(") symbol';
  sClosePathensisExpected = '")" expected but "," found';
  sOpenPathensisExpected  = '"(" expected but ")" or "," found';
  sExpressionExcepted = 'Expression expected but end of expression found';
  sNotOperatorMissied = 'Expression expected after not operator but do not found';
  sOperatorsMissed = 'Expression expected after and before operator but do not found';
  sInvalidExpression = 'Invalid expression';
  sInvalidCharInExpression = 'Unexpected symbol "%s" in expression "%s"';
  sInvalidVarDeclaration = 'Invalid variable declaration "%s" in if block';
  sElseIfAfterElseBlock = 'Elseif block do not allowed after else';
  sElseAfterElseBlock = 'Only one else block allowed';
  sForEachElseAfterBlockEnd = 'Foreachselse block do not allowed after closed foreach block';
  sFromVariableRequireForEach = 'From parameter required in foreach or capturearray block';
  sDuplicateModifierName = 'Duplicate modifier name';
  sDuplicateFunctionName = 'Duplicate function name';
  sDuplicateNamespaceName = 'Duplicate namespace name';
  sInvalidFunctionDeclaration = 'Invalid function "%s" declaration';
  sOpenBraceInTemplate = 'Unexpected "{" symbol';
  sLiteralDoNotFound = 'Unclosed {literal} direclaration found';
  sInvalidTemplateDirective = 'Invalid directive "%s" in template';
  sUncloseQuote = 'Quote ''"'' do not have corresponding (''"'' ) symbol';
  sCloseBraceWithoutTemplate = 'Unexpected "}" symbol';
  sBraceDoNotClose = 'Expected "}" but do not found';
  sAtPosition = ' at line: %d; position: %d';
  sIntegerExceedRangeSigned = 'Integer constant %d exceeds range';
  sIntegerExceedRangeUnsigned = 'Integer constant %u exceeds range';

implementation

var
 SmartyProvider: TSmartyInfoProvider;

{   = packed record
    Year: Word;         //0 means undefine
    Month, Day: byte;     }

function TDateRecord.GetVariant: Variant;
begin
  Result := VarArrayCreate([0, 2], varInteger);
  Result[0] := Year;
  Result[1] := Month;
  Result[2] := Day;
end;

procedure TDateRecord.SetVariant(AValue: Variant);
begin
  if VarIsArray(AValue) then
  begin
    try Year := AValue[0]; except Year := 0; end;
    try Month := AValue[1]; except Month := 0; end;
    try Day := AValue[2]; except Day := 0; end;
  end;
end;

function GetDateRecordVariant(AYear: Word; AMonth: Word = 0; ADay: Word = 0): Variant;
begin
  Result := VarArrayCreate([0, 2], varInteger);
  Result[0] := AYear;
  Result[1] := AMonth;
  Result[2] := ADay;
end;

function TryStringToBool(AValue: string; var B: Boolean): Boolean;
begin
  Result := True;
  if (CompareText('true', AValue) = 0) or (CompareText('1', AValue) = 0) then B := True
  else if (CompareText('false', AValue) = 0) or (CompareText('0', AValue) = 0) then B := False
  else Result := False;
end;

function Arr(const Values: array of TVariableRecord): TVariableRecord;
var I: Integer;
begin
  Result := Default(TVariableRecord);
  Result.SetArrayLength(Length(Values));
  for I := Low(Values) to High(Values) do
    Result.SetArrayItemQ(I, '', Values[I]);
end;

function Map(const Items: array of TVariableArrayItem): TVariableRecord;
var I: Integer;
begin
  Result := Default(TVariableRecord);
  Result.SetArrayLength(Length(Items));
  for I := Low(Items) to High(Items) do
    Result.SetArrayItemQ(I, Items[I].Key, Items[I].Item);
end;

function Item(const Key: string; const Value: TVariableRecord): TVariableArrayItem;
begin
  Result.Key := Key;
  Result.Item := Value;
end;

function Item(const Key: string; const Values: array of TVariableRecord): TVariableArrayItem; overload;
begin
  Result := Item(Key, Arr(Values));
end;

function Item(const Key: string; Items: array of TVariableArrayItem): TVariableArrayItem; overload;
begin
  Result := Item(Key, Map(Items));
end;

procedure RegisterModifier(AModifier: TVariableModifierClass);
begin
  SmartyProvider.AddModifier(AModifier);
end;

procedure RegisterFunction(AFunction: TSmartyFunctionClass);
begin
  SmartyProvider.AddFunction(AFunction);
end;

procedure UnregisterFunction(AFunction: TSmartyFunctionClass);
begin
  SmartyProvider.DeleteFunction(AFunction);
end;

function SmartyExec(const Pattern, NamespaceName: string; const Variables: array of TVariableArrayItem;
                    out Errors, Output: string): Boolean;
var
  Namesp: TStorageNamespaceProvider;
  Smarty: TSmartyEngine;
begin
  Namesp := TStorageNamespaceProvider.Create(NamespaceName);
  Smarty := TSmartyEngine.Create;
  try
    Namesp.SetVariables(Variables);
    Smarty.AddNamespace(Namesp);
    Result := Smarty.Execute(Pattern, Errors, Output);
  finally
    FreeAndNil(Smarty);
  end;
end;

const
  MaxPrecedence = 11;
  OperatorPrecedence: array[TOperator] of byte = (
    6, {opEq}
    6, {opNeq}
    5, {opGt}
    5, {opLt}
     5, {opGte}
    5, {opLte}
    6, {opSEq}
    3, {opAdd}
    3, {opSub}
    2, {opMply}
    2, {opDivide}
    2, {opMod}
    2, {opDiv}
    4, {opShl}
    4, {opShr}
    1, {opLogicalNot}
    10, {opLogicalAnd}
    11, {opLogicalOr}
    1, {opBitwiseNot}
    7, {opBitwiseAnd}
    9, {opBitwiseOr}
    8  {opBitwiseXor}
  );

function IsSpace(C: Char): Boolean; inline;
begin
  Result := (C <= ' ') or TCharacter.IsWhiteSpace(C);
end;

function GetChar(S: string; Index: Integer): Char; inline;
begin
  if Index <= Length(S) then
    Result := S[Index]
  else
    Result := #0;
end;

function StartsWithSpace(const ASubStr, AStr: string): Boolean;
begin
  Result := StartsStr(ASubStr, AStr) and (Length(AStr) > Length(ASubStr))
    and IsSpace(AStr[Length(ASubStr) + 1]);
end;

function DateTimeFromString(const ADate: string): TDateTime;
// Convert the string ADate to a TDateTime according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
var
  AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: Word;
begin
  AYear  := StrToInt(Copy(ADate, 1, 4));
  AMonth := StrToInt(Copy(ADate, 6, 2));
  ADay   := StrToInt(Copy(ADate, 9, 2));
  if Length(ADate) > 16 then
  begin
    AHour := StrToInt(Copy(ADate, 12, 2));
    AMin  := StrToInt(Copy(ADate, 15, 2));
    ASec  := StrToIntDef(Copy(ADate, 18, 2), 0); // They might be omitted, so default to 0
    AMSec := StrToIntDef(Copy(ADate, 21, 3), 0); // They might be omitted, so default to 0
  end
  else begin
    AHour := 0;
    AMin  := 0;
    ASec  := 0;
    AMSec := 0;
  end;
  Result := EncodeDate(AYear, AMonth, ADay) +
    EncodeTime(AHour, AMin, ASec, AMSec);
end;

function DateLooseFromString(const ADate: string): TDateRecord;
begin
  Result.Year := StrToIntDef(Copy(ADate, 1, 4), 0);
  Result.Month := StrToIntDef(Copy(ADate, 6, 2), 0);
  Result.Day := StrToIntDef(Copy(ADate, 9, 2), 0);
end;

function ParseEscapes(const AStr: string): string;
var
  I, J: Integer;
  Ch: Char;
  Hex: Integer;

  function IsHexChar(const S: string; Index: Integer; var HexValue: Integer): Boolean; inline;
  var
    hCh: Char;
  begin
    if Index <= Length(S) then
    begin
      Result := True;
      hCh := S[Index];

      case hCh of
        '0'..'9': HexValue := HexValue * 16 + Ord(hCh) - Ord('0');
        'a'..'f': HexValue := HexValue * 16 + Ord(hCh) - Ord('a') + 10;
        'A'..'F': HexValue := HexValue * 16 + Ord(hCh) - Ord('A') + 10;
      else
        Result := False;
      end;

    end
    else
      Result := False;
  end;

begin
  Result := '';
  I := 1;

  while I <= Length(AStr) do
  begin
    Ch := AStr[I];
    Inc(I);

    if (Ch = '\') and (I <= Length(AStr)) then
    begin
      Ch := AStr[I];
      Inc(I);

      if Ch = 'n' then Result := Result + #10
      else if Ch = 'r' then Result := Result + #13
      else if Ch = 't' then Result := Result + #9
      else if Ch = 'a' then Result := Result + #7
      else if Ch = 'b' then Result := Result + #8
      else if Ch = 'v' then Result := Result + #11
      else if Ch = 'e' then Result := Result + #27
      else if Ch = 'f' then Result := Result + #12
      else if Ch = '\' then Result := Result + '\'
      else if Ch = 'x' then
      begin
        Inc(I);
        J := 1;
        Hex := 0;

        while IsHexChar(AStr, I, Hex) and (J <= 6) do
        begin
          Inc(J);
          Inc(I);
        end;

        if Hex <> 0 then
          if Hex >= $10000 then
          begin
            Result := Result + Char(((Hex - $10000) div $400) + $d800) +
              Char(((Hex - $10000) and $3ff) + $dc00);
          end
          else
            Result := Result + Chr((Hex));
      end;
    end
    else
      Result := Result + Ch;
  end;
end;

function IsTrimCharacter(Ch: Char): Boolean; inline;
begin
  Result := (Ch <= ' ') or TCharacter.IsWhiteSpace(Ch);
end;

function SmartyTrim(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  if (L > 0) and not IsTrimCharacter(S[I]) and  not IsTrimCharacter(S[L]) then Exit(S);
  while (I <= L) and IsTrimCharacter(S[I]) do Inc(I);
  if I > L then Exit('');
  while IsTrimCharacter(S[L]) do Dec(L);
  Result := Copy(S, I, L - I + 1);
end;

function SmartyTrimLeft(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and IsTrimCharacter(S[I]) do Inc(I);
  if I = 1 then Exit(S);
  Result := Copy(S, I, Maxint);
end;

function SmartyTrimRight(const S: string): string;
var
  I: Integer;
begin
  I := Length(S);
  if (I > 0) and not IsTrimCharacter(S[I]) then Exit(S);
  while (I > 0) and IsTrimCharacter(S[I]) do Dec(I);
  Result := Copy(S, 1, I);
end;

function IsLetterSymbol(C: Char): Boolean; inline;
begin
  if (C = '''') or (C = #$2019) then
    Result := True
  else
    case TCharacter.GetUnicodeCategory(C) of
      TUnicodeCategory.ucUppercaseLetter,
      TUnicodeCategory.ucLowercaseLetter,
      TUnicodeCategory.ucTitlecaseLetter,
      TUnicodeCategory.ucModifierLetter,
      TUnicodeCategory.ucOtherLetter,
      TUnicodeCategory.ucNonSpacingMark,
      TUnicodeCategory.ucDashPunctuation:
        Result := True;
    else
      Result := False;
    end;
end;

function UCWords(const AStr: string; ACapitalizeDigits: Boolean = False): string;
var
  I, J: Integer;
  WasDelimiter, IsNumber: Boolean;
  Ch, iCh: Char;
  S: string;
begin
  Result := '';
  WasDelimiter := True;
  I := 1;

  while I <= Length(AStr) do
  begin
    Ch := AStr[I];
    Inc(I);

    if WasDelimiter and TCharacter.IsLetter(Ch) then
    begin
      if ACapitalizeDigits then
      begin
        Result := Result + TCharacter.ToUpper(Ch);
        WasDelimiter := False;
      end
      else begin
        S := '';

        IsNumber := False;
        J := I;
        while J <= Length(AStr) do
        begin
          iCh := AStr[J];
          Inc(J);
          I := J;

          if TCharacter.IsDigit(iCh) then
          begin
            IsNumber := True;
            S := S + iCh;
          end
          else if TCharacter.IsLetter(iCh) or (iCh = '''') or (iCh = #$2019) then
            S := S + iCh
          else begin
            I := J - 1;
            Break;
          end;
        end;

        if IsNumber then
          Result := Result + Ch + S
        else
          Result := Result + TCharacter.ToUpper(Ch) + S;
        WasDelimiter := False;
      end;
    end
    else begin
      Result := Result + Ch;
      WasDelimiter := not (TCharacter.IsDigit(Ch) or TCharacter.IsLetter(Ch) or (Ch = '''') or (Ch = #$2019));
    end;
  end;
end;

function CountCharacters(const AStr: string; ACountWhitespace: Boolean = False): Integer;
var
  I: Integer;
begin
  if ACountWhitespace then
    Result := Length(AStr)
  else begin
    Result := 0;
    for I := 1 to Length(AStr) do
      if not TCharacter.IsWhiteSpace(AStr, I) then Inc(Result);
  end;
end;

function TruncateString(const AStr: string; ALength: Integer = 80; AEtc: string = '...';
  ABreakWords: Boolean = False; AMiddle: Boolean = False): string;
var
  I, L: Integer;
begin
  Result := SmartyTrim(AStr);

  if Length(Result) > ALength then
  begin
    L := ALength div 2;

    if not AMiddle then
    begin
      if ABreakWords then
        Result := Copy(Result, 1, ALength) + AEtc
      else begin
        Result := Copy(Result, 1, ALength + 1);

        I := Length(Result);

        //skip last Word up to half of length
        while I > L do
          if IsLetterSymbol(Result[I]) then
            Dec(I)
          else
            Break;

        //skip delimiters up to half of length
        while I > L do
          if not IsLetterSymbol(Result[I]) then
            Dec(I)
          else
            Break;

        Result := Copy(Result, 1, I) + AEtc;
      end;
    end
    else
      Result := Copy(AStr, 1, L) + AEtc + Copy(AStr, Length(AStr) - L + 1, L);
  end;
end;

function Strip(const AStr: string; AStripStr: string = ' '): string;
var
  I: Integer;
  WasWhiteSpace: Boolean;
  Ch: Char;
begin
  Result := '';
  WasWhiteSpace := False;
  for I := 1 to Length(AStr) do
  begin
    Ch := AStr[I];
    if TCharacter.IsWhiteSpace(Ch) then
      WasWhiteSpace := True
    else begin
      if WasWhiteSpace then
      begin
        WasWhiteSpace := False;
        Result := Result + AStripStr;
      end;

      Result := Result + Ch;
    end;
  end;

  if WasWhiteSpace then Result := Result + AStripStr;
end;

//idea for strip_tags from php sources
function StripTags(const AStr: string; ANoSpace: Boolean = True;
  AParse: Boolean = True): string;
var
  Br, I, Depth: Integer;
  State, Len: Integer;
  Ch, LastChar, In_Quote: Char;
  S: string;

  function GetIC(S: string; Index, Len: Integer): Char; inline;
  begin
    if (Index > 0) and (Index <= Len) then
      Result := S[Index]
    else
      Result := #0;
  end;

begin
  Result := '';
  S := Strip(AStr);

  I := 0;
  Br := 0;
  Depth := 0;
  In_Quote := #0;
  State := 0;
  LastChar := #0;
  Len := Length(S);

  while I < Len do
  begin
    Inc(I);
    Ch := S[I];

    case Ch of
      '<':
      begin
        if In_Quote <> #0 then
          Continue
        else if State = 0 then
        begin
          LastChar := '<';
          State := 1;

          //check parse tags
          if AParse then
          begin
            if ((GetIC(S, I+1, Len) = '/') and (ToLower(GetIC(S, I+2, Len)) = 'p')) or       //</p
              ((ToLower(GetIC(S, I+1, Len)) = 'b') and (ToLower(GetIC(S, I+2, Len)) = 'r'))  //<br
            then
              Result := Result + #13#10
            else
              if not ANoSpace then Result := Result + ' ';
          end
          else
            if not ANoSpace then Result := Result + ' ';
        end
        else if State = 1 then
        begin
          Inc(Depth);
        end;
      end;

      '(':
      begin
        if State = 2 then
        begin
          if (LastChar <> '"') and (LastChar <> '''') then
          begin
            LastChar := '(';
            Inc(Br);
          end;
        end
        else if State = 0 then
          Result := Result + Ch;
      end;

      ')':
      begin
        if State = 2 then
        begin
          if (LastChar <> '"') and (LastChar <> '''') then
          begin
            LastChar := ')';
            Dec(Br);
          end;

        end
        else if State = 0 then
          Result := Result + Ch;
      end;

      '>':
      begin
        if Depth > 0 then
        begin
          Dec(Depth);
          Continue;
        end;

        if In_Quote <> #0 then
          Continue;

        case State of
          1: // HTML/XML
          begin
            LastChar := '>';
            In_Quote := #0;
            State := 0;
          end;

          2: // PHP
          if (Br = 0) and (LastChar <> '"') and (GetIC(S, I-1, Len) = '?') then
          begin
            In_Quote := #0;
            State := 0;
          end;

          3: //JavaScript/CSS/etc...
          begin
            In_Quote := #0;
            State := 0;
          end;

          4: //Inside <!-- comment -->
          begin
            if (GetIC(S, I-1, Len) = '-') and  (GetIC(S, I-2, Len) = '-') then
            begin
              In_Quote := #0;
              State := 0;
            end;
          end;

        else
          Result := Result + Ch;
        end;

      end;

      '"', '\':
      begin
        if State = 4 then  //Inside <!-- comment -->
          Continue
        else if (State = 2) and (GetIC(S, I-1, Len) = '\') then
        begin
          if LastChar = Ch then
            LastChar := #0
          else if LastChar <> '\' then
            LastChar := Ch;
        end
        else if State = 0 then
          Result := Result + Ch;


        if (State > 0) and (I > 1) and
          ((State = 1) or (GetIC(S, I-1, Len) <> '\')) and
          ((In_Quote = #0) or (Ch = In_Quote)) then
        begin
          if In_Quote <> #0 then
            In_Quote := #0
          else
            In_Quote := Ch;
        end;
      end;

      '!':
      begin
        //JavaScript & Other HTML scripting languages
        if (State = 1) and (GetIC(S, I-1, Len) = '<') then
        begin
          State := 3;
          LastChar := Ch;
        end
        else if State = 0 then
          Result := Result + Ch;
      end;

      '-':
      begin
        if (State = 3) and (GetIC(S, I-1, Len) = '-') and (GetIC(S, I-2, Len) = '!') then
          State := 4
        else if State = 0 then
          Result := Result + Ch;
      end;

      '?':
      begin
        if (State = 1) and (GetIC(S, I-1, Len) = '<') then
        begin
          Br := 0;
          State := 2;
        end
        else if State = 0 then
          Result := Result + Ch;
      end;

      'E', 'e':
      begin
        if (State = 3) and  //!DOCTYPE exception
          (ToLower(GetIC(S, I-1, Len)) = 'p') and
          (ToLower(GetIC(S, I-2, Len)) = 'y') and
          (ToLower(GetIC(S, I-3, Len)) = 't') and
          (ToLower(GetIC(S, I-4, Len)) = 'c') and
          (ToLower(GetIC(S, I-5, Len)) = 'o') and
          (ToLower(GetIC(S, I-6, Len)) = 'd') then
        begin
          State := 1;
        end
        else if State = 0 then
          Result := Result + Ch;
      end;

      'l', 'L':
      begin
        // If we encounter '<?xml' then we shouldn't be in
        // state == 2 (PHP). Switch back to HTML.
        if (State = 2) and
          (ToLower(GetIC(S, I-1, Len)) = 'm') and
          (ToLower(GetIC(S, I-2, Len)) = 'x') then
        begin
          State := 1;
        end
        else if State = 0 then
          Result := Result + Ch;
      end;

    else
      if State = 0 then Result := Result + Ch;
    end;
  end;
end;

function Spacify(const AStr: string; ASpacifyStr: string = ' '): string;
var
  I, L: Integer;
begin
  if Length(AStr) > 0 then
  begin
    Result := '';
    L := Length(AStr);
    for I := 1 to L do
      if I <> L then
        Result := Result + AStr[I] + ASpacifyStr
      else
        Result := Result + AStr[I];
  end
  else
    Result := '';
end;

function Wordwrap(const Line: string; MaxCol: Integer = 80;
  const BreakStr: string = sLineBreak): string;
const
  QuoteChars = ['''', '"'];
var
  Col, Pos: Integer;
  LinePos, LineLen: Integer;
  BreakLen, BreakPos: Integer;
  QuoteChar, CurChar: Char;
  ExistingBreak: Boolean;
  L: Integer;

  function IsBreakChar(Ch: Char): Boolean; inline;
  begin
    Result := TCharacter.IsWhiteSpace(Ch) or (Ch = #$0009);
  end;

begin
  Col := 1;
  Pos := 1;
  LinePos := 1;
  BreakPos := 0;
  QuoteChar := #0;
  ExistingBreak := False;
  LineLen := Length(Line);
  BreakLen := Length(BreakStr);
  Result := '';

  while Pos <= LineLen do
  begin
    CurChar := Line[Pos];
    if IsLeadChar(CurChar) then
    begin
      {$IFDEF FPC}
      {$message 'TODO'}
      {$ELSE}
      L := CharLength(Line, Pos) div SizeOf(Char) - 1;
      {$ENDIF}
      Inc(Pos, L);
      Inc(Col, L);
    end
    else
    begin
      if CharInSet(CurChar, QuoteChars) then
        if QuoteChar = #0 then
          QuoteChar := CurChar
        else if CurChar = QuoteChar then
          QuoteChar := #0;
      if QuoteChar = #0 then
      begin
        if CurChar = BreakStr[1] then
        begin
          ExistingBreak := StrLComp(PChar(BreakStr), PChar(@Line[Pos]), BreakLen) = 0;
          if ExistingBreak then
          begin
            Inc(Pos, BreakLen-1);
            BreakPos := Pos;
          end;
        end;

        if not ExistingBreak then
          if IsBreakChar(CurChar) then
            BreakPos := Pos;
      end;
    end;

    Inc(Pos);
    Inc(Col);

    if not CharInSet(QuoteChar, QuoteChars) and (ExistingBreak or
      ((Col > MaxCol) and (BreakPos > LinePos))) then
    begin
      Col := 1;
      //Result := Result + Copy(Line, LinePos, BreakPos - LinePos + 1);
      Result := Result + Copy(Line, LinePos, BreakPos - LinePos);
      if not CharInSet(CurChar, QuoteChars) then
      begin
        while Pos <= LineLen do
        begin
          if IsBreakChar(Line[Pos]) then
          begin
            Inc(Pos);
            ExistingBreak := False;
          end
          else
          begin
            if StrLComp(PChar(@Line[Pos]), PChar(sLineBreak), Length(sLineBreak)) = 0 then
            begin
              Inc(Pos, Length(sLineBreak));
              ExistingBreak := True;
            end
            else
              Break;
          end;
        end;
      end;
      if (Pos <= LineLen) and not ExistingBreak then
        Result := Result + BreakStr;

      Inc(BreakPos);
      LinePos := BreakPos;
      Pos := LinePos;
      ExistingBreak := False;
    end;
  end;
  Result := Result + Copy(Line, LinePos, MaxInt);
end;

function IsLineBreak(C: Char): Boolean; inline;
begin
  if Integer(C) <= $FF then
    Result := (C = #$000A) or (C = #$000D) or (C = #$0085)
  else
    case TCharacter.GetUnicodeCategory(C) of
      TUnicodeCategory.ucLineSeparator,
      TUnicodeCategory.ucParagraphSeparator: Result := True
    else
      Result := False;
    end;
end;

function CountParagraphs(const AStr: string): Integer;
var
  WasLineBreak: Boolean;
  Ch: Char;
  I: Integer;
begin
  Result := 1;
  WasLineBreak := False;
  for I := 1 to Length(AStr) do
  begin
    Ch := AStr[I];
    if IsLineBreak(Ch) then
      WasLineBreak := True
    else if WasLineBreak then
    begin
      Inc(Result);
      WasLineBreak := False;
    end;
  end;
end;

function CountWords(const AStr: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  I := 1;
  while I <= Length(AStr) do
  begin
    while (I <= Length(AStr)) and not TCharacter.IsLetter(AStr[I]) do Inc(I);

    if I <= Length(AStr) then
    begin
      Inc(Result);
      while (I <= Length(AStr)) and IsLetterSymbol(AStr[I]) do Inc(I);
    end;
  end;
end;

function IndentString(const AStr: string; IndentStr: string = ' '): string;
var
  I: Integer;
  Ch: Char;
  WasLineBreak: Boolean;
begin
  Result := '';
  WasLineBreak := False;
  for I := 1 to Length(AStr) do
  begin
    Ch := AStr[I];

    if IsLineBreak(Ch) then
      WasLineBreak := True
    else if WasLineBreak then
    begin
      Result := Result + IndentStr;
      WasLineBreak := False;
    end;

    Result := Result + Ch;
  end;
end;

function XMLEncode(const AStr: string): string;
var
  I: Integer;
  Ch: Char;
begin
  Result := '';
  for I := 1 to Length(AStr) do
  begin
    Ch := AStr[I];
    case Ch of
      '&': Result := Result + '&amp;';
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '''': Result := Result + '&apos;';
      '"': Result := Result + '&quot;';
    else
      Result := Result + Ch;
    end;
  end;
end;

function HTMLEncode(const AStr: string): string;
var
  I: Integer;
  Ch: Char;
begin
  Result := '';
  for I := 1 to Length(AStr) do
  begin
    Ch := AStr[I];
    case Ch of
      '&': Result := Result + '&amp;';
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '''': Result := Result + '&#39;';
      '"': Result := Result + '&quot;';
      #160: Result := Result + '&nbsp;';
    else
      Result := Result + Ch;
    end;
  end;
end;

function HTMLEncodeEntities(const AStr: string): string;
var
  I: Integer;
  Ch: Char;
begin
  Result := '';
  for I := 1 to Length(AStr) do
  begin
    Ch := AStr[I];
    case WideChar(Ch) of // FPC requires, Delphi - noop
      '&': Result := Result + '&amp;';
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '''': Result := Result + '&#39;';
      '"': Result := Result + '&quot;';
      #160: Result := Result + '&nbsp;';
      #161: Result := Result + '&iexcl';
      #162: Result := Result + '&cent;';
      #163: Result := Result + '&pound;';
      #164: Result := Result + '&curren;';
      #165: Result := Result + '&yen;';
      #166: Result := Result + '&brvbar;';
      #167: Result := Result + '&sect;';
      #168: Result := Result + '&uml;';
      #169: Result := Result + '&copy;';
      #170: Result := Result + '&ordf;';
      #171: Result := Result + '&laquo;';
      #172: Result := Result + '&not;';
      #173: Result := Result + '&shy;';
      #174: Result := Result + '&reg;';
      #175: Result := Result + '&macr;';
      #176: Result := Result + '&deg;';
      #177: Result := Result + '&plusmn;';
      #178: Result := Result + '&sup2;';
      #179: Result := Result + '&sup3;';
      #180: Result := Result + '&acute;';
      #181: Result := Result + '&micro;';
      #182: Result := Result + '&para;';
      #183: Result := Result + '&middot;';
      #184: Result := Result + '&cedil;';
      #185: Result := Result + '&sup1;';
      #186: Result := Result + '&ordm;';
      #187: Result := Result + '&raquo;';
      #188: Result := Result + '&frac14;';
      #189: Result := Result + '&frac12;';
      #190: Result := Result + '&frac34;';
      #191: Result := Result + '&iquest;';
      #192: Result := Result + '&Agrave;';
      #193: Result := Result + '&Aacute;';
      #194: Result := Result + '&Acirc;';
      #195: Result := Result + '&Atilde;';
      #196: Result := Result + '&Auml;';
      #197: Result := Result + '&Aring;';
      #198: Result := Result + '&AElig;';
      #199: Result := Result + '&Ccedil;';
      #200: Result := Result + '&Egrave;';
      #201: Result := Result + '&Eacute;';
      #202: Result := Result + '&Ecirc;';
      #203: Result := Result + '&Euml;';
      #204: Result := Result + '&Igrave;';
      #205: Result := Result + '&Iacute;';
      #206: Result := Result + '&Icirc;';
      #207: Result := Result + '&Iuml;';
      #208: Result := Result + '&ETH;';
      #209: Result := Result + '&Ntilde;';
      #210: Result := Result + '&Ograve;';
      #211: Result := Result + '&Oacute;';
      #212: Result := Result + '&Ocirc;';
      #213: Result := Result + '&Otilde;';
      #214: Result := Result + '&Ouml;';
      #215: Result := Result + '&times;';
      #216: Result := Result + '&Oslash;';
      #217: Result := Result + '&Ugrave;';
      #218: Result := Result + '&Uacute;';
      #219: Result := Result + '&Ucirc;';
      #220: Result := Result + '&Uuml;';
      #221: Result := Result + '&Yacute;';
      #222: Result := Result + '&THORN;';
      #223: Result := Result + '&szlig;';
      #224: Result := Result + '&agrave;';
      #225: Result := Result + '&aacute;';
      #226: Result := Result + '&acirc;';
      #227: Result := Result + '&atilde;';
      #228: Result := Result + '&auml;';
      #229: Result := Result + '&aring;';
      #230: Result := Result + '&aelig;';
      #231: Result := Result + '&ccedil;';
      #232: Result := Result + '&egrave;';
      #233: Result := Result + '&eacute;';
      #234: Result := Result + '&ecirc;';
      #235: Result := Result + '&euml;';
      #236: Result := Result + '&igrave;';
      #237: Result := Result + '&iacute;';
      #238: Result := Result + '&icirc;';
      #239: Result := Result + '&iuml;';
      #240: Result := Result + '&eth;';
      #241: Result := Result + '&ntilde;';
      #242: Result := Result + '&ograve;';
      #243: Result := Result + '&oacute;';
      #244: Result := Result + '&ocirc;';
      #245: Result := Result + '&otilde;';
      #246: Result := Result + '&ouml;';
      #247: Result := Result + '&divide;';
      #248: Result := Result + '&oslash;';
      #249: Result := Result + '&ugrave;';
      #250: Result := Result + '&uacute;';
      #251: Result := Result + '&ucirc;';
      #252: Result := Result + '&uuml;';
      #253: Result := Result + '&yacute;';
      #254: Result := Result + '&thorn;';
      #255: Result := Result + '&yuml;';
      #338: Result := Result + '&OElig;';
      #339: Result := Result + '&oelig;';
      #352: Result := Result + '&Scaron;';
      #353: Result := Result + '&scaron;';
      #376: Result := Result + '&Yuml;';
      #402: Result := Result + '&fnof;';
      #710: Result := Result + '&circ;';
      #732: Result := Result + '&tilde;';
      #913: Result := Result + '&Alpha;';
      #914: Result := Result + '&Beta;';
      #915: Result := Result + '&Gamma;';
      #916: Result := Result + '&Delta;';
      #917: Result := Result + '&Epsilon;';
      #918: Result := Result + '&Zeta;';
      #919: Result := Result + '&Eta;';
      #920: Result := Result + '&Theta;';
      #921: Result := Result + '&Iota;';
      #922: Result := Result + '&Kappa;';
      #923: Result := Result + '&Lambda;';
      #924: Result := Result + '&Mu;';
      #925: Result := Result + '&Nu;';
      #926: Result := Result + '&Xi;';
      #927: Result := Result + '&Omicron;';
      #928: Result := Result + '&Pi;';
      #929: Result := Result + '&Rho;';
      #931: Result := Result + '&Sigma;';
      #932: Result := Result + '&Tau;';
      #933: Result := Result + '&Upsilon;';
      #934: Result := Result + '&Phi;';
      #935: Result := Result + '&Chi;';
      #936: Result := Result + '&Psi;';
      #937: Result := Result + '&Omega;';
      #945: Result := Result + '&alpha;';
      #946: Result := Result + '&beta;';
      #947: Result := Result + '&gamma;';
      #948: Result := Result + '&delta;';
      #949: Result := Result + '&epsilon;';
      #950: Result := Result + '&zeta;';
      #951: Result := Result + '&eta;';
      #952: Result := Result + '&theta;';
      #953: Result := Result + '&iota;';
      #954: Result := Result + '&kappa;';
      #955: Result := Result + '&lambda;';
      #956: Result := Result + '&mu;';
      #957: Result := Result + '&nu;';
      #958: Result := Result + '&xi;';
      #959: Result := Result + '&omicron;';
      #960: Result := Result + '&pi;';
      #961: Result := Result + '&rho;';
      #962: Result := Result + '&sigmaf;';
      #963: Result := Result + '&sigma;';
      #964: Result := Result + '&tau;';
      #965: Result := Result + '&upsilon;';
      #966: Result := Result + '&phi;';
      #967: Result := Result + '&chi;';
      #968: Result := Result + '&psi;';
      #969: Result := Result + '&omega;';
      #977: Result := Result + '&thetasym;';
      #978: Result := Result + '&upsih;';
      #982: Result := Result + '&piv;';
      #8194: Result := Result + '&ensp;';
      #8195: Result := Result + '&emsp;';
      #8201: Result := Result + '&thinsp;';
      #8204: Result := Result + '&zwnj;';
      #8205: Result := Result + '&zwj;';
      #8206: Result := Result + '&lrm;';
      #8207: Result := Result + '&rlm;';
      #8211: Result := Result + '&ndash;';
      #8212: Result := Result + '&mdash;';
      #8216: Result := Result + '&lsquo;';
      #8217: Result := Result + '&rsquo;';
      #8218: Result := Result + '&sbquo;';
      #8220: Result := Result + '&ldquo;';
      #8221: Result := Result + '&rdquo;';
      #8222: Result := Result + '&bdquo;';
      #8224: Result := Result + '&dagger;';
      #8225: Result := Result + '&Dagger;';
      #8226: Result := Result + '&bull;';
      #8230: Result := Result + '&hellip;';
      #8240: Result := Result + '&permil;';
      #8242: Result := Result + '&prime;';
      #8243: Result := Result + '&Prime;';
      #8249: Result := Result + '&lsaquo;';
      #8254: Result := Result + '&oline;';
      #8250: Result := Result + '&rsaquo;';
      #8260: Result := Result + '&frasl;';
      #8364: Result := Result + '&euro;';
      #8472: Result := Result + '&weierp;';
      #8465: Result := Result + '&image;';
      #8476: Result := Result + '&real;';
      #8482: Result := Result + '&trade;';
      #8501: Result := Result + '&alefsym;';
      #8592: Result := Result + '&larr;';
      #8593: Result := Result + '&uarr;';
      #8594: Result := Result + '&rarr;';
      #8595: Result := Result + '&darr;';
      #8596: Result := Result + '&harr;';
      #8629: Result := Result + '&crarr;';
      #8656: Result := Result + '&lArr;';
      #8657: Result := Result + '&uArr;';
      #8658: Result := Result + '&rArr;';
      #8659: Result := Result + '&dArr;';
      #8660: Result := Result + '&hArr;';
      #8704: Result := Result + '&forall;';
      #8706: Result := Result + '&part;';
      #8707: Result := Result + '&exist;';
      #8709: Result := Result + '&empty;';
      #8711: Result := Result + '&nabla;';
      #8712: Result := Result + '&isin;';
      #8713: Result := Result + '&notin;';
      #8715: Result := Result + '&ni;';
      #8719: Result := Result + '&prod;';
      #8721: Result := Result + '&sum;';
      #8722: Result := Result + '&minus;';
      #8727: Result := Result + '&lowast;';
      #8730: Result := Result + '&radic;';
      #8733: Result := Result + '&prop;';
      #8734: Result := Result + '&infin;';
      #8736: Result := Result + '&ang;';
      #8743: Result := Result + '&and;';
      #8744: Result := Result + '&or;';
      #8745: Result := Result + '&cap;';
      #8746: Result := Result + '&cup;';
      #8747: Result := Result + '&int;';
      #8756: Result := Result + '&there4;';
      #8764: Result := Result + '&sim;';
      #8773: Result := Result + '&cong;';
      #8776: Result := Result + '&asymp;';
      #8800: Result := Result + '&ne;';
      #8801: Result := Result + '&equiv;';
      #8804: Result := Result + '&le;';
      #8805: Result := Result + '&ge;';
      #8834: Result := Result + '&sub;';
      #8835: Result := Result + '&sup;';
      #8836: Result := Result + '&nsub;';
      #8838: Result := Result + '&sube;';
      #8839: Result := Result + '&supe;';
      #8853: Result := Result + '&oplus;';
      #8855: Result := Result + '&otimes;';
      #8869: Result := Result + '&perp;';
      #8901: Result := Result + '&sdot;';
      #8968: Result := Result + '&lceil;';
      #8969: Result := Result + '&rceil;';
      #8970: Result := Result + '&lfloor;';
      #8971: Result := Result + '&rfloor;';
      #9001: Result := Result + '&lang;';
      #9002: Result := Result + '&rang;';
      #9674: Result := Result + '&loz;';
      #9824: Result := Result + '&spades;';
      #9827: Result := Result + '&clubs;';
      #9829: Result := Result + '&hearts;';
      #9830: Result := Result + '&diams;';
    else
      Result := Result + Ch;
    end;
  end;
end;

function FileEncode(const S: string): string;
var
  I: Integer;
begin
  Result := 'file:///';
  for I := 1 to Length(S) do
  begin
    if S[I] = '\' then
      Result := Result + '/'
    else if CharInSet(S[I], ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.', '/', ':']) then
      Result := Result + S[I]
    else
      Result := Result + '%' + IntToHex(Ord(S[I]), 2);
  end;
end;

{************* TVariableRecord *************}

procedure TVariableRecord.Finalize;
var
  ArrayData: PVariableArray;
  I: Integer;
begin
  case Self.VarType of
    vtString:
    begin
      string(Self.SValue) := '';
      Self.SValue := nil;
    end;
    vtArray:
    begin
      ArrayData := Self.AValue;
      if ArrayData.Count > 0 then
      begin
        for I := 0 to ArrayData.Count - 1 do
        begin
          ArrayData.Data[I].Key := '';
          ArrayData.Data[I].Item.Finalize;
        end;
        FreeMem(ArrayData.Data, ArrayData.Count * SizeOf(TVariableArrayItem));
      end;
      FreeMem(Self.AValue, SizeOf(TVariableArray));
    end;
  end;
end;

function TVariableRecord.Clone: TVariableRecord;
var
  ArrayData: PVariableArray;
  I: Integer;
begin
  Result := Default(TVariableRecord);

  Result.VarType := Self.VarType;
  case Self.VarType of
    vtNull: ;
    vtBoolean:
      Result.BValue := Self.BValue;
    vtInt:
      Result.IValue := Self.IValue;
    vtFloat:
      Result.FValue := Self.FValue;
    vtDateStrict:
      Result.DSValue := Self.DSValue;
    vtDateLoose:
      Result.DLValue := Self.DLValue;
    vtDateTime:
      Result.DTValue := Self.DTValue;
    vtString:
    begin
      Result.SValue := nil;
      string(Result.SValue) := string(Self.SValue);
    end;
    vtArray:
    begin
      Result.AValue := nil;
      Result.AValue := AllocMem(SizeOf(TVariableArray));
      ArrayData := Result.AValue;
      PVariableArray(Result.AValue).Count := PVariableArray(Self.AValue).Count;
      PVariableArray(Result.AValue).Reference := PVariableArray(Self.AValue).Reference;
      if PVariableArray(Self.AValue).Count > 0 then
      begin
        ArrayData.Data := AllocMem(ArrayData.Count * SizeOf(TVariableArrayItem));
        for I := 0 to ArrayData.Count - 1 do
        begin
          ArrayData.Data[I].Key := PVariableArray(Self.AValue).Data[I].Key;
          ArrayData.Data[I].Item := PVariableArray(Self.AValue).Data[I].Item.Clone;
        end;
      end
      else
        ArrayData.Data := nil;
    end;
  end;
end;

function TVariableRecord.IsNull: Boolean;
begin
  Result := (Self.VarType = vtNull);
end;

function TVariableRecord.IsEmpty: Boolean;
begin
  case Self.VarType of
    vtNull:
      Result := True;
    vtBoolean:
      Result := False;
    vtInt:
      Result := (Self.IValue = 0);
    vtFloat:
      Result := (Self.FValue = 0);
    vtDateStrict:
      Result := (Self.DSValue = 0);
    vtDateLoose:
      Result := (Self.DLValue.Year = 0) and  (Self.DLValue.Month = 0) and (Self.DLValue.Day = 0);
    vtDateTime:
      Result := (Self.DTValue = 0);
    vtString:
      Result := (string(Self.SValue) = '');
    vtArray:
      Result := (PVariableArray(Self.AValue).Count = 0);
  else
    Result := True;
  end;
end;

function TVariableRecord.IsArray: Boolean;
begin
  Result := (Self.VarType = vtArray);
end;

function TVariableRecord.IsBoolean: Boolean;
begin
  Result := (Self.VarType = vtBoolean);
end;

function TVariableRecord.IsInt: Boolean;
begin
  Result := (Self.VarType = vtInt);
end;

function TVariableRecord.IsFloat: Boolean;
begin
  Result := (Self.VarType = vtFloat);
end;

function TVariableRecord.IsNumber: Boolean;
begin
  Result := (Self.VarType = vtInt) or (Self.VarType = vtFloat);
end;

function TVariableRecord.IsDateStrict: Boolean;
begin
  Result := (Self.VarType = vtDateStrict);
end;

function TVariableRecord.IsDateLoose: Boolean;
begin
  Result := (Self.VarType = vtDateLoose);
end;

function TVariableRecord.IsDateTime: Boolean;
begin
  Result := (Self.VarType = vtDateTime);
end;

function TVariableRecord.IsDate: Boolean;
begin
  Result := (Self.VarType = vtDateStrict) or (Self.VarType = vtDateLoose) or
    (Self.VarType = vtDateTime);
end;

function TVariableRecord.IsString: Boolean;
begin
  Result := (Self.VarType = vtString);
end;

class function TVariableRecord.Null: TVariableRecord;
begin
  Result.VarType := vtNull;
end;

class function TVariableRecord.AsInt(AValue: Int64; ANullValue: Int64): TVariableRecord;
begin
  if AValue <> ANullValue then
    Result := AValue
  else
    Result.VarType := vtNull;
end;

class function TVariableRecord.AsFloat(AValue: Double; ANullValue: Double): TVariableRecord;
begin
  if AValue <> ANullValue then
    Result := AValue
  else
    Result.VarType := vtNull;
end;

class function TVariableRecord.AsString(const AValue: string; const ANullValue: string): TVariableRecord;
begin
  if AValue <> ANullValue then
    Result := AValue
  else
    Result.VarType := vtNull;
end;

class function TVariableRecord.AsDateRecord(const AValue: TDateRecord): TVariableRecord;
begin
  if (AValue.Year <> 0) or (AValue.Month <> 0) or (AValue.Day <> 0) then
    Result := AValue
  else
    Result.VarType := vtNull;
end;

procedure TVariableRecord.SetNull;
begin
  Finalize;
  VarType := vtNull;
end;

procedure TVariableRecord.SetBool(AValue: Boolean);
begin
  Finalize;
  VarType := vtBoolean;
  BValue := AValue;
end;

procedure TVariableRecord.SetInt(AValue: Int64);
begin
  Finalize;
  VarType := vtInt;
  IValue := AValue;
end;

procedure TVariableRecord.SetFloat(Avalue: Double);
begin
  Finalize;
  VarType := vtFloat;
  FValue := AValue;
end;

procedure TVariableRecord.SetString(const AValue: string);
begin
  Finalize;
  VarType := vtString;
  SValue := nil;
  string(SValue) := AValue;
end;

procedure TVariableRecord.SetArrayLength(AValue: Integer;
  AReference: TObject = nil; AInit: Boolean = False);
var
  ArrayData: PVariableArray;
  I: Integer;
begin
  VarType := vtArray;
  Self.AValue := nil;
  Self.AValue := AllocMem(SizeOf(TVariableArray));
  ArrayData := Self.AValue;
  ArrayData.Data := AllocMem(AValue * SizeOf(TVariableArrayItem));
  ArrayData.Count := AValue;
  ArrayData.Reference := AReference;

  if AInit then
    for I := 0 to AValue - 1 do
    begin
      ArrayData.Data[I].Key := '';
      ArrayData.Data[I].Item := TVariableRecord.Null;
    end;
end;

procedure TVariableRecord.SetArrayItem(AIndex: Integer;
  const AKey: string; const AValue: TVariableRecord);
var
  ArrayData: PVariableArray;
begin
  ArrayData := Self.AValue;
  if (AIndex >= 0) and (AIndex < ArrayData.Count) then
  begin
    ArrayData.Data[AIndex].Key := DoValidIdent(AKey);
    ArrayData.Data[AIndex].Item := AValue;
  end
  else
    raise ESmartyException.CreateRes(@sIncorrectArrayItem);
end;

procedure TVariableRecord.SetArrayItemQ(AIndex: Integer;
  const AKey: string; const AValue: TVariableRecord);
var
  ArrayData: PVariableArray;
begin
  ArrayData := Self.AValue;
  Assert((AIndex >= 0) and (AIndex < ArrayData.Count), 'Invalid array item');
  if (AKey = '') or IsValidIdent(AKey) then
  begin  
    ArrayData.Data[AIndex].Key := AKey;
    ArrayData.Data[AIndex].Item := AValue;
  end
  else
    raise ESmartyException.CreateRes(@sIncorrectArrayKey);
end;

class operator TVariableRecord.Implicit(AValue: Boolean): TVariableRecord;
begin
  Result.VarType := vtBoolean;
  Result.BValue := AValue;
end;

class operator TVariableRecord.Implicit(AValue: Integer): TVariableRecord;
begin
  Result := Int64(AValue);
end;

class operator TVariableRecord.Implicit(AValue: Cardinal): TVariableRecord;
begin
  Result := Int64(AValue);
end;

class operator TVariableRecord.Implicit(AValue: Int64): TVariableRecord;
begin
  Result.VarType := vtInt;
  Result.IValue := AValue;
end;

// UInt64: check if the value will be truncated on type cast

class operator TVariableRecord.Implicit(AValue: UInt64): TVariableRecord;
begin
  if AValue > High(Int64) then
    raise ESmartyException.CreateResFmt(@sIntegerExceedRangeUnsigned, [AValue]);
  Result := Int64(AValue);
end;

class operator TVariableRecord.Implicit(AValue: Double): TVariableRecord;
begin
  Result.VarType := vtFloat;
  Result.FValue := AValue;
end;

class operator TVariableRecord.Implicit(AValue: Extended): TVariableRecord;
begin
  Result.VarType := vtFloat;
  Result.FValue := AValue;
end;

class operator TVariableRecord.Implicit(AValue: TDate): TVariableRecord;
begin
  Result.VarType := vtDateStrict;
  Result.DSValue := AValue;
end;

class operator TVariableRecord.Implicit(const AValue: TDateRecord): TVariableRecord;
begin
  Result.VarType := vtDateLoose;
  Result.DLValue := AValue;
end;

class operator TVariableRecord.Implicit(AValue: TDateTime): TVariableRecord;
begin
  Result.VarType := vtDateTime;
  Result.DTValue := AValue;
end;

class operator TVariableRecord.Implicit(const AValue: string): TVariableRecord;
begin
  Result.VarType := vtString;
  Result.SValue := nil;
  string(Result.SValue) := AValue;
end;

class operator TVariableRecord.Implicit(const AValue: Variant): TVariableRecord;
begin
  case TVarData(AValue).VType of
    varEmpty     : Result.Null;
    varNull      : Result.Null;
    varSmallint  : Result := TVarData(AValue).VSmallInt;
    varInteger   : Result := TVarData(AValue).VInteger;
    varDouble    : Result := TVarData(AValue).VDouble;
    varCurrency  : Result := TVarData(AValue).VCurrency;
    varDate      : Result := TVarData(AValue).VDate;
    varOleStr    : Result := string(TVarData(AValue).VOleStr);
    varBoolean   : Result := TVarData(AValue).VBoolean;
    varShortInt  : Result := TVarData(AValue).VShortInt;
    varByte      : Result := TVarData(AValue).VByte;
    varWord      : Result := TVarData(AValue).VWord;
    varLongWord  : Result := TVarData(AValue).VLongWord;
    varInt64     : Result := TVarData(AValue).VInt64;
    varUInt64    : Result := TVarData(AValue).{$IFDEF DCC}VInt64{$ENDIF} {$IFDEF FPC}vqword{$ENDIF};
    varString    : Result := string(TVarData(AValue).VString);
    varUString   : Result := string(TVarData(AValue).VUString);
  end;
end;

class operator TVariableRecord.Implicit(const ARecord: TVariableRecord): Boolean;
begin
  Result := ARecord.ToBool;
end;

class operator TVariableRecord.Implicit(const ARecord: TVariableRecord): Integer;
begin
  Result := ARecord.ToInt;
end;

class operator TVariableRecord.Implicit(const ARecord: TVariableRecord): Double;
begin
  Result := ARecord.ToFloat;
end;

class operator TVariableRecord.Implicit(const ARecord: TVariableRecord): string;
begin
  Result := ARecord.ToString;
end;

function TVariableRecord.ToBool: Boolean;
var
  I: Integer;
  ArrayData: PVariableArray;
{$IF NOT DECLARED(DefaultFalseBoolStr)}
const
  DefaultFalseBoolStr = 'False'; // DO NOT LOCALIZE
{$IFEND}
begin
  case Self.VarType of
    vtNull:
      Result := False;
    vtBoolean:
      Result := Self.BValue;
    vtInt:
      Result := (Self.IValue <> 0);
    vtFloat:
      Result := (Self.FValue <> 0);
    vtDateStrict:
      Result := (Self.DSValue <> 0);
    vtDateLoose:
      Result := (Self.DLValue.Year <> 0) or (Self.DLValue.Month <> 0) or
      (Self.DLValue.Day <> 0);
    vtDateTime:
      Result := (Self.DTValue <> 0);
    vtString:
    begin
      Result := (string(Self.SValue) <> '') and (string(Self.SValue) <> '0') and
        not AnsiSameText(string(Self.SValue), DefaultFalseBoolStr);
    end;
    vtArray:
    begin
      ArrayData := Self.AValue;
      Result := ArrayData.Count > 0;
      if Result then
      begin
        Result := False;
        for I := 0 to ArrayData.Count - 1 do
        begin
          Result := Result or ArrayData.Data[I].Item.ToBool;
          if Result then Break;
        end;
      end;
    end
  else
    Result := False;
  end;
end;

function TVariableRecord.ToInt: Int64;
begin
  case Self.VarType of
    vtNull:
      Result := 0;
    vtBoolean:
      if Self.BValue then Result := 1 else Result := 0;
    vtInt:
      Result := Self.IValue;
    vtFloat:
      Result := Round(Self.FValue);
    vtDateStrict:
      Result := Round(Self.DSValue);
    vtDateLoose:
      Result := Round(DateTimeFromRecord(Self.DLValue));
    vtDateTime:
      Result := Round(Self.DTValue);
    vtString:
      Result := StrToIntDef(string(Self.SValue), 0);
    vtArray:
      if ToBool then Result := 1 else Result := 0;
  else
    Result := 0;
  end;
end;

function TVariableRecord.ToFloat: Double;
begin
  case Self.VarType of
    vtNull:
      Result := 0;
    vtBoolean:
      if Self.BValue then Result := 1 else Result := 0;
    vtInt:
      Result := Self.IValue;
    vtFloat:
      Result := Self.FValue;
    vtDateStrict:
      Result := Self.DSValue;
    vtDateLoose:
      Result := DateTimeFromRecord(Self.DLValue);
    vtDateTime:
      Result := Self.DTValue;
    vtString:
      Result := StrToFloatDef(string(Self.SValue), 0);
    vtArray:
      if ToBool then Result := 1 else Result := 0;
  else
    Result := 0;
  end;
end;

function TVariableRecord.ToString: string;
begin
  case Self.VarType of
    vtNull:
      Result := '';
    vtBoolean:
      if Self.BValue then Result := '1' else Result := '';
    vtInt:
      Result := IntToStr(Self.IValue);
    vtFloat:
      Result := FloatToStr(Self.FValue);
    vtDateStrict:
      Result := DateToStr(Self.DSValue);
    vtDateLoose:
      Result := DateRecordToStr(Self.DLValue);
    vtDateTime:
      Result := DateTimeToStr(Self.DTValue);
    vtString:
      Result := string(Self.SValue);
    vtArray:
      Result := 'Array';
  end;
end;

function TVariableRecord.CanConvertToLogical(out Value: Boolean): Boolean;
begin
  case Self.VarType of
    vtNull:
      Result := False;
    vtBoolean:
    begin
      Value := Self.BValue;
      Result := True;
    end;
    vtInt, vtFloat, vtDateStrict, vtDateLoose, vtDateTime, vtArray:
      Result := False;
    vtString:
      Result := TryStrToBool(string(Self.SValue), Value);
  else
    Result := False;
  end;
end;

function TVariableRecord.CanConvertToInt(out Value: Int64): Boolean;
begin
  case Self.VarType of
    vtNull:
      Result := False;
    vtBoolean:
    begin
      if Self.BValue then Value := 1 else Value := 0;
      Result := True;
    end;
    vtInt:
    begin
      Value := Self.IValue;
      Result := True;
    end;
    vtDateStrict:
    begin
      Value := Round(Self.DSValue);
      Result := True;
    end;
    vtFloat, vtDateLoose, vtDateTime, vtArray:
      Result := False;
    vtString:
      Result := TryStrToInt64(string(Self.SValue), Value);
  else
    Result := False;
  end;
end;

function TVariableRecord.CanConvertToFloat(out Value: Double): Boolean;
begin
  case Self.VarType of
    vtNull, vtArray:
      Result := False;
    vtBoolean:
    begin
      if Self.BValue then Value := 1 else Value := 0;
      Result := True;
    end;
    vtInt:
    begin
      Value := Self.IValue;
      Result := True;
    end;
    vtFloat:
    begin
      Value := Self.FValue;
      Result := True;
    end;
    vtDateStrict:
    begin
      Value := Self.DSValue;
      Result := True;
    end;
    vtDateLoose:
    begin
      Value := DateTimeFromRecord(Self.DLValue);
      Result := True;
    end;
    vtDateTime:
    begin
      Value := Self.DTValue;
      Result := True;
    end;
    vtString:
      Result := TryStrToFloat(string(Self.SValue), Value);
  else
    Result := False;
  end;
end;

class function TVariableRecord.DoCompareRelationship(const ALeft, ARight: TVariableRecord;
  AOperation: TCompareOperation): TVariableRelatioship;
var
  CompareType: TVariableType;
  B1, B2: Boolean;
  I, SCompare: Integer;
  I1, I2: Int64;
  F1, F2, DCompare: Double;
  S1, S2: string;
  DL: TDateRecord;
  StartTime, EndTime: TDateTime;
  VA1, VA2: PVariableArray;
const
  CBaseTypes: array [TVariableType, TVariableType] of TVariableType =
           {Null}     {Bool}     {Int}       {Float}      {Date}      {DateLoose}   {DateTime}    {String}      {Array}
{Null} ((vtNull,    vtBoolean, vtBoolean,   vtBoolean,   vtBoolean,    vtBoolean,   vtBoolean,   vtBoolean,    vtBoolean),
{Bool}  (vtBoolean, vtBoolean, vtBoolean,   vtBoolean,   vtBoolean,    vtBoolean,   vtBoolean,   vtBoolean,    vtBoolean),
{Int}   (vtBoolean, vtBoolean, vtInt,       vtFloat,     vtInt,        vtDateLoose, vtFloat,     vtFloat,      vtInt),
{Float} (vtBoolean, vtBoolean, vtFloat,     vtFloat,     vtFloat,      vtDateLoose, vtFloat,     vtFloat,      vtFloat),
{Date}  (vtBoolean, vtBoolean, vtInt,       vtFloat,     vtDateStrict, vtDateLoose, vtDateTime,  vtDateStrict, vtDateStrict),
{DateL} (vtBoolean, vtBoolean, vtDateLoose, vtDateLoose, vtDateLoose,  vtDateLoose, vtDateLoose, vtDateLoose,  vtDateLoose),
{DateT} (vtBoolean, vtBoolean, vtFloat,     vtFloat,     vtDateTime,   vtDateLoose, vtDateTime,  vtDateTime,   vtDateTime),
{Str}   (vtBoolean, vtBoolean, vtFloat,     vtFloat,     vtDateStrict, vtDateLoose, vtDateTime,  vtString,     vtString),
{Array} (vtBoolean, vtBoolean, vtInt,       vtFloat,     vtDateStrict, vtDateLoose, vtDateTime,  vtString,     vtArray));
begin
  if AOperation <> coSEq then
  begin
    CompareType := CBaseTypes[ALeft.VarType, ARight.VarType];
     case CompareType of
      vtNull:
        Result := vrEqual;

      vtBoolean:
      begin
        B1 := ALeft.ToBool;
        B2 := ARight.ToBool;
        if B1 > B2 then Result := vrGreaterThan
        else if B1 = B2 then Result := vrEqual
        else Result := vrLessThan;
      end;

      vtDateStrict, vtInt:
      begin
        I1 := ALeft.ToInt;
        I2 := ARight.ToInt;
        if I1 > I2 then Result := vrGreaterThan
        else if I1 = I2 then Result := vrEqual
        else Result := vrLessThan;
      end;

      vtFloat, vtDateTime:
      begin
        F1 := ALeft.ToFloat;
        F2 := ARight.ToFloat;
        if F1 > F2 then Result := vrGreaterThan
        else if F1 = F2 then Result := vrEqual
        else Result := vrLessThan;
      end;

      vtDateLoose:
      begin
        if ALeft.VarType = vtDateLoose then
        begin
          DL := ALeft.DLValue;
          DCompare := ARight.ToFloat;
          StartTime := GetStartDate(DL);
          EndTime := GetEndDate(DL);
          if StartTime > DCompare then
            Result := vrGreaterThan
          else if EndTime < DCompare then
            Result := vrLessThan
          else
            Result := vrEqual;
        end
        else begin
          DL := ARight.DLValue;
          DCompare := ALeft.ToFloat;
          StartTime := GetStartDate(DL);
          EndTime := GetEndDate(DL);
          if DCompare < StartTime then
            Result := vrLessThan
          else if DCompare > EndTime then
            Result := vrGreaterThan
          else
            Result := vrEqual;
        end;
      end;

      vtString:
      begin
        S1 := ALeft.ToString;
        S2 := ARight.ToString;
        SCompare := CompareStr(S1, S2);
        if SCompare > 0 then Result := vrGreaterThan
        else if SCompare = 0 then Result := vrEqual
        else Result := vrLessThan;
      end;

      vtArray: 
      begin
        I1 := PVariableArray(ALeft.AValue).Count;
        I2 := PVariableArray(ALeft.AValue).Count;
        if I1 > I2 then Result := vrGreaterThan
        else if I1 = I2 then Result := vrEqual
        else Result := vrLessThan;
      end;
    else
      Result := vrEqual;
    end;

  end
  else begin
    //SEq operation
    if ALeft.VarType <> ARight.VarType then
      Result := vrGreaterThan
    else begin
      case ALeft.VarType of
        vtNull:
          Result := vrEqual;

        vtBoolean:
          if ALeft.BValue = ARight.BValue then
            Result := vrEqual
          else
            Result := vrGreaterThan;

        vtInt:
          if ALeft.IValue = ARight.IValue then
            Result := vrEqual
          else
            Result := vrGreaterThan;

        vtFloat:
          if ALeft.FValue = ARight.FValue then
            Result := vrEqual
          else
            Result := vrGreaterThan;

        vtDateStrict:
          if ALeft.DSValue = ARight.DSValue then
            Result := vrEqual
          else
            Result := vrGreaterThan;

        vtDateLoose:
        begin
          if (ALeft.DLValue.Year = ARight.DLValue.Year) and
             (ALeft.DLValue.Month = ARight.DLValue.Month) and
             (ALeft.DLValue.Day = ARight.DLValue.Day) then
            Result := vrEqual
          else
            Result := vrGreaterThan;
        end;

        vtDateTime:
          if ALeft.DTValue = ARight.DTValue then
            Result := vrEqual
          else
            Result := vrGreaterThan;

        vtString:
          if string(ALeft.SValue) = string(ARight.SValue) then
            Result := vrEqual
          else
            Result := vrGreaterThan;

        vtArray: 
        begin
          VA1 := PVariableArray(ALeft.AValue);
          VA2 := PVariableArray(ALeft.AValue);
          if VA1.Count = VA2.Count then
          begin
            Result := vrEqual;

            if VA1.Count > 0 then            
              for I := 0 to VA1.Count - 1 do
              begin
                Result := DoCompareRelationship(VA1.Data[I].Item, VA2.Data[I].Item, AOperation);
                if Result <> vrEqual then Break;               
              end
          end
          else
            Result := vrGreaterThan;
        end;
      else
        Result := vrEqual;
      end;
    end;
  end;
end;

class function TVariableRecord.DoCompare(const ALeft, ARight: TVariableRecord;
  AOperation: TCompareOperation): Boolean;
const
  CRelationshipToBoolean: array [TCompareOperation, TVariableRelatioship] of Boolean =
  //  vrGreaterThan, vrEqual, vrLessThan
    ((False, True,  False),  // coEq
     (True,  False, True),   // coNeq
     (True,  False, False),  // coGt
     (False, False, True),   // coLt
     (True,  True,  False),  // coGte
     (False, True,  True),   // coLte
     (False, True,  False)); // coSEq
var
  Realationship: TVariableRelatioship;
begin
  Realationship := DoCompareRelationship(ALeft, ARight, AOperation);
  Result := CRelationshipToBoolean[AOperation, Realationship];
end;

class function TVariableRecord.DoIntFloatOp(const ALeft, ARight: TVariableRecord;
  AOperation: TBinaryOperation): TVariableRecord;
var
  I1, I2: Int64;
  F1, F2: Double;
  CanI1, CanI2, CanF1, CanF2: Boolean;
begin
  CanI1 := ALeft.CanConvertToInt(I1);
  CanI2 := ARight.CanConvertToInt(I2);
  if CanI1 and CanI2 then
  begin
    case AOperation of
      voAdd:
        Result := I1 + I2;
      voSubtract:
        Result := I1 - I2;
      voMultiply:
        Result := I1 * I2;
    end;
  end
  else begin
    CanF1 := ALeft.CanConvertToFloat(F1);
    CanF2 := ARight.CanConvertToFloat(F2);
    if CanF1 and CanF2 then
    begin
      case AOperation of
        voAdd:
          Result := F1 + F2;
        voSubtract:
          Result := F1 - F2;
        voMultiply:
          Result := F1 * F2;
      end;
    end
    else begin
      if CanI1 or CanI2 then
      begin
        if not CanI1 then I1 := ALeft.ToInt;
        if not CanI2 then I2 := ARight.ToInt;
        case AOperation of
          voAdd:
            Result := I1 + I2;
          voSubtract:
            Result := I1 - I2;
          voMultiply:
            Result := I1 * I2;
        end;
      end
      else if CanF1 or CanF2 then
      begin
        if not CanF1 then F1 := ALeft.ToFloat;
        if not CanF2 then F2 := ARight.ToFloat;
        case AOperation of
          voAdd:
            Result := F1 + F2;
          voSubtract:
            Result := F1 - F2;
          voMultiply:
            Result := F1 * F2;
        end;
      end
      else begin
        if not CanI1 then I1 := ALeft.ToInt;
        if not CanI2 then I2 := ARight.ToInt;
        case AOperation of
          voAdd:
            Result := I1 + I2;
          voSubtract:
            Result := I1 - I2;
          voMultiply:
            Result := I1 * I2;
        end;
      end;
    end;
  end;
end;

class function TVariableRecord.DoFloatOp(const ALeft, ARight: TVariableRecord;
  AOperation: TBinaryOperation): TVariableRecord;
var
  F1, F2: Double;
  CanF1, CanF2: Boolean;
begin
  CanF1 := ALeft.CanConvertToFloat(F1);
  CanF2 := ARight.CanConvertToFloat(F2);
  if CanF1 and CanF2 then
  begin
    if AOperation = voDivide then Result := F1 / F2;
  end
  else begin
    if not CanF1 then F1 := ALeft.ToFloat;
    if not CanF2 then F2 := ARight.ToFloat;
    if AOperation = voDivide then
      if F2 <> 0 then
        Result := F1 / F2
      else
        Result := TVariableRecord.Null;
  end;
end;

class function TVariableRecord.DoIntOp(const ALeft, ARight: TVariableRecord;
  AOperation: TBinaryOperation): TVariableRecord;
var
  I1, I2: Int64;
  CanI1, CanI2: Boolean;
begin
  CanI1 := ALeft.CanConvertToInt(I1);
  CanI2 := ARight.CanConvertToInt(I2);
  if CanI1 and CanI2 then
  begin
    case AOperation of
      voAnd:
        Result := I1 and I2;
      voOr:
        Result := I1 or I2;
      voXor:
        Result := I1 xor I2;
      voIntDivide:
        Result := I1 div I2;
      voModulus:
        Result := I1 mod I2;
      voShl:
        Result := I1 shl I2;
      voShr:
        Result := I1 shr I2;
    end;
  end
  else begin
    if not CanI1 then I1 := ALeft.ToInt;
    if not CanI2 then I2 := ARight.ToInt;
    case AOperation of
      voAnd:
        Result := I1 and I2;
      voOr:
        Result := I1 or I2;
      voXor:
        Result := I1 xor I2;
      voIntDivide:
        Result := I1 div I2;
      voModulus:
        Result := I1 mod I2;
      voShl:
        Result := I1 shl I2;
      voShr:
        Result := I1 shr I2;
    end;
  end;
end;

class function TVariableRecord.DoIntNot(const ARight: TVariableRecord): TVariableRecord;
begin
  Result := not ARight.ToInt;
end;

class function TVariableRecord.DoLogicalOp(const ALeft, ARight: TVariableRecord;
  AOperation: TBinaryOperation): TVariableRecord;
var
  B1, B2: Boolean;
  CanB1, CanB2: Boolean;
begin
  CanB1 := ALeft.CanConvertToLogical(B1);
  CanB2 := ARight.CanConvertToLogical(B2);
  if not CanB1 then B1 := ALeft.ToBool;
  if not CanB2 then B2 := ARight.ToBool;
  case AOperation of
    voAnd:
      Result := B1 and B2;
    voOr:
      Result := B1 or B2;
    voXor:
      Result := B1 xor B2;
  end;
end;

class function TVariableRecord.DoLogicalNot(const ARight: TVariableRecord): TVariableRecord;
begin
  Result := not ARight.ToBool;
end;

class operator TVariableRecord.Add(const ALeft, ARight: TVariableRecord): TVariableRecord;
begin
  DoIntFloatOp(ALeft, ARight, voAdd);
end;

class operator TVariableRecord.Subtract(const ALeft, ARight: TVariableRecord): TVariableRecord;
begin
  DoIntFloatOp(ALeft, ARight, voSubtract);
end;

class operator TVariableRecord.Multiply(const ALeft, ARight: TVariableRecord): TVariableRecord;
begin
  DoIntFloatOp(ALeft, ARight, voMultiply);
end;

class operator TVariableRecord.Divide(const ALeft, ARight: TVariableRecord): TVariableRecord;
begin
  DoFloatOp(ALeft, ARight, voDivide);
end;

class operator TVariableRecord.IntDivide(const ALeft, ARight: TVariableRecord): TVariableRecord;
begin
  DoIntOp(ALeft, ARight, voIntDivide);
end;

class operator TVariableRecord.Modulus(const ALeft, ARight: TVariableRecord): TVariableRecord;
begin
  DoIntOp(ALeft, ARight, voModulus);
end;

class operator TVariableRecord.LeftShift(const ALeft, ARight: TVariableRecord): TVariableRecord;
begin
  DoIntOp(ALeft, ARight, voShl);
end;

class operator TVariableRecord.RightShift(const ALeft, ARight: TVariableRecord): TVariableRecord;
begin
  DoIntOp(ALeft, ARight, voShr);
end;

class operator TVariableRecord.LogicalAnd(const ALeft, ARight: TVariableRecord): TVariableRecord;
begin
  DoLogicalOp(ALeft, ARight, voAnd);
end;

class operator TVariableRecord.LogicalOr(const ALeft, ARight: TVariableRecord): TVariableRecord;
begin
  DoLogicalOp(ALeft, ARight, voOr);
end;

class operator TVariableRecord.LogicalXor(const ALeft, ARight: TVariableRecord): TVariableRecord;
begin
  DoLogicalOp(ALeft, ARight, voXor);
end;

{************* TVariablePart *************}

procedure TVariablePart.Finalize;
begin
  case Self.PartType of
    vptValue:
      string(Self.SValue) := '';
  end;
end;

function TVariablePart.Clone: TVariablePart;
begin
  Result.PartType := Self.PartType;
  case Self.PartType of
    vptValue:
    begin
      Result.SValue := nil;
      string(Result.SValue) := string(Self.SValue);  
    end;
    vptIndex:
      Result.IValue := Self.IValue;
  end;
end;

class operator TVariablePart.Implicit(AValue: Int64): TVariablePart;
begin
  Result.PartType := vptIndex;
  Result.IValue := AValue;
end;

class operator TVariablePart.Implicit(const AValue: string): TVariablePart;
begin
  Result.PartType := vptValue;
  Result.SValue := nil;
  string(Result.SValue) := AValue;
end;

class operator TVariablePart.Implicit(const APart: TVariablePart): Integer;
begin
  case APart.PartType of
    vptIndex: Result := APart.IValue;
  else
    Result := -1;
  end;
end;

class operator TVariablePart.Implicit(const APart: TVariablePart): string;
begin
  case APart.PartType of
    vptValue: Result := string(APart.SValue);
  else
    Result := '';
  end;
end;

{************* TVarList *************}

function TVarList.Clone: TVarList;
var
  I: Integer;
begin
  Result := TVarList.Create;
   for I := 0 to Count - 1 do Result.Add(Items[I].Clone);
end;

procedure TVarList.Finalize;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do Items[I].Finalize;
  Free;
end;

procedure TVarList.DeleteElement(Index: Integer);
begin
  Items[Index].Finalize;
  Delete(Index);
end;

procedure TVarList.AddArrayPrefix(AVariable: TVarList; Index: Integer);
var
  I: Integer;
begin
  for I := 0 to AVariable.Count - 1 do
    Insert(I, AVariable[I]);
  Insert(AVariable.Count, Index);
end;

function TVarList.IsSimpleVariable(out VarName: string): Boolean;
begin
  if (Count = 1) and (Items[0].PartType = vptValue) then
  begin
    VarName := Items[0];
    Result := True;
  end
  else
    Result := False;
end;

function TVarList.CheckTopLevel(const AName: string): Boolean;
begin
  if (Count >= 1) and (Items[0].PartType = vptValue) and
    (CompareText(Items[0], AName) = 0) then
  begin
    Result := True;
    DeleteElement(0);
  end
  else
    Result := False;
end;

function TVarList.IsTopValueLevel(out AName: string): Boolean;
begin
  if (Count >= 1) and (Items[0].PartType = vptValue) then
  begin
    AName := Items[0];
    Result := True;
    DeleteElement(0);
  end
  else
    Result := False;
end;

{************* TNamespaceProvider *************}

constructor TNamespaceProvider.Create(const AName: string; UseCache,
  IsIndexSupported: Boolean; Min, Max: Integer);
begin
  FName := AName;
  FIsIndexSupported := IsIndexSupported;
  FUseCache := UseCache;
  FMin := Min;
  FMax := Max;
  inherited Create;
end;

function TNamespaceProvider.GetName: string;
begin
  Result := FName;
end;

function TNamespaceProvider.IsIndexSupported: Boolean;
begin
  Result := FIsIndexSupported;
end;

function TNamespaceProvider.UseCache: Boolean;
begin
  Result := FUseCache;
end;

procedure TNamespaceProvider.GetIndexProperties(var AMin, AMax: Integer);
begin
  AMin := FMin;
  AMax := FMax;
end;

{************* TStorageNamespaceProvider *************}

destructor TStorageNamespaceProvider.Destroy;
var VarRec: TVariableRecord;
begin
  if FVariables <> nil then
    for VarRec in FVariables.Values do
      VarRec.Finalize;
  FreeAndNil(FVariables);
  inherited;
end;

procedure TStorageNamespaceProvider.InternalSetVariable(const AVarName: string; const Value: TVariableRecord);
begin
  // lazy init, also saves us from overriding 2 constructors
  if FVariables = nil then
    FVariables := TDictionary<string,TVariableRecord>.Create;
  // ! Clone the record so that the class owns its personal copy and does with it whatever it wants
  FVariables.AddOrSetValue(AnsiUpperCase(AVarName), Value.Clone);
end;

function TStorageNamespaceProvider.InternalGetVariable(const AVarName: string): TVariableRecord;
begin
  if not FVariables.ContainsKey(AVarName) then
    Result := TVariableRecord.Null
  else
    Result := FVariables[AVarName].Clone;
end;

function TStorageNamespaceProvider.GetVariable(AIndex: Integer; const AVarName: string): TVariableRecord;
begin
  Result := InternalGetVariable(AVarName);
end;

procedure TStorageNamespaceProvider.ClearVariables;
begin
  FVariables.Clear;
end;

procedure TStorageNamespaceProvider.SetVariable(const AVarName: string; const Value: TVariableRecord);
begin
  InternalSetVariable(AVarName, Value);
end;

procedure TStorageNamespaceProvider.SetVariable(const AVarName: string; const Values: array of TVariableRecord);
begin
  InternalSetVariable(AVarName, Arr(Values));
end;

procedure TStorageNamespaceProvider.SetVariables(const Variables: array of TVariableArrayItem);
var
  I: Integer;
begin
  for I := Low(Variables) to High(Variables) do
    SetVariable(Variables[I].Key, Variables[I].Item);
end;

{************* TForEachList *************}

constructor TForEachList.Create;
begin
  inherited Create;
  CurrentRecords := TList<Integer>.Create;
end;

destructor TForEachList.Destroy;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do Items[I].Free;
  CurrentRecords.Free;
  inherited Destroy;
end;

procedure TForEachList.EnterForEach(AList: TForEachData);
begin
  CurrentRecords.Add(Add(AList));
end;

procedure TForEachList.ExitForEach;
begin
  if (CurrentRecords.Count > 0) then
    CurrentRecords.Delete(CurrentRecords.Count - 1);
end;

function TForEachList.InForEach: Boolean;
begin
  Result := CurrentRecords.Count > 0;
end;

function TForEachList.FindItemRecord(const AItemName: string; out ARecord: TForEachData): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := CurrentRecords.Count - 1 downto 0 do
    if CompareText(AItemName, Items[CurrentRecords[I]].ItemVarName) = 0 then
    begin
      ARecord := Items[CurrentRecords[I]];
      Exit(True);
    end;
end;

function TForEachList.FindKeyRecord(const AKeyName: string; out ARecord: TForEachData): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := CurrentRecords.Count - 1 downto 0 do
    if CompareText(AKeyName, Items[CurrentRecords[I]].KeyVarName) = 0 then
    begin
      ARecord := Items[CurrentRecords[I]];
      Exit(True);
    end;
end;

function TForEachList.FindRecord(const AName: string;
  out ARecord: TForEachData): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (CompareText(AName, 'current') = 0) and (CurrentRecords.Count > 0) then
  begin
    ARecord := Items[CurrentRecords[CurrentRecords.Count-1]];
    Result := True;
  end
  else if AName <> '' then
  begin
    for I := 0 to Count - 1 do
      if CompareText(Items[I].Name, AName) = 0 then
      begin
        ARecord := Items[I];
        Exit(True);
      end;
  end;
end;

{************* TCaptureArrayItem *************}

constructor TCaptureArrayItem.Create;
begin
  inherited Create;
  IsActive := False;
  ItemName := '';
  Index := 0;
  VarData := nil;
end;

destructor TCaptureArrayItem.Destroy;
begin
  inherited Destroy;
end;

procedure TCaptureArrayItem.Enter(const AName: string; AIndex: Integer; AVarData: PVariableArray);
begin
  IsActive := True;
  ItemName := AName;
  Index := AIndex;
  VarData := AVarData;
end;

procedure TCaptureArrayItem.IncIndex;
begin
  Inc(Index);
end;

procedure TCaptureArrayItem.Exit;
begin
  IsActive := False;
  VarData := nil;
end;

function TCaptureArrayItem.IsItemName(const AName: string): Boolean;
begin
  Result := IsActive and (CompareText(ItemName, AName) = 0);
end;


{************* TSmartyProvider *************}

constructor TSmartyProvider.Create(AEngine: TSmartyEngine);
begin
  inherited Create;
  FEngine := AEngine;
  FForEachList := TForEachList.Create;
  FCaptureCache := TList<TCaptureCache>.Create;
  FActiveCapture := TCaptureArrayItem.Create;
end;

destructor TSmartyProvider.Destroy;
begin
  ClearCaptureCache;
  FCaptureCache.Free;
  FForEachList.Free;
  FActiveCapture.Free;
  inherited Destroy;
end;

procedure TSmartyProvider.ClearCaptureCache;
var
  I: Integer;
begin
  for I := FCaptureCache.Count - 1 downto 0 do
  begin
    FCaptureCache[I].VariableValue^.Finalize;
    FreeMem(FCaptureCache[I].VariableValue, SizeOf(TVariableRecord));
    FCaptureCache.Delete(I);
  end;
end;

function TSmartyProvider.FindCaptureItem(const AName: string; var Cache: TCaptureCache): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FCaptureCache.Count - 1 do
  begin
    Cache := FCaptureCache[I];
    if CompareText(Cache.VariableName, AName) = 0 then Exit(True);
  end;
end;

procedure TSmartyProvider.SetCaptureItem(const AName: string; const VariableValue: TVariableRecord);
var
  Cache: TCaptureCache;
begin
  if FindCaptureItem(AName, Cache) then
  begin
    Cache.VariableValue^.Finalize;
    Cache.VariableValue^ := VariableValue;
  end
  else begin
    Cache.VariableValue := AllocMem(SizeOf(TVariableRecord));
    Cache.VariableName := AName;
    Cache.VariableValue^ := VariableValue;
    FCaptureCache.Add(Cache);
  end;
end;

procedure TSmartyProvider.RemoveCaptureItem(const AName: string);
var
  Cache: TCaptureCache;
  I: Integer;
begin
  for I := 0 to FCaptureCache.Count - 1 do
  begin
    Cache := FCaptureCache[I];
    if CompareText(Cache.VariableName, AName) = 0 then
    begin
      Cache.VariableValue^.Finalize;
      FreeMem(Cache.VariableValue, SizeOf(TVariableRecord));
      FCaptureCache.Delete(I);
      Exit;
    end;
  end;
end;

function TSmartyProvider.GetName: string;
begin
  Result := 'smarty';
end;

function TSmartyProvider.IsIndexSupported: Boolean;
begin
  Result := False;
end;

function TSmartyProvider.UseCache: Boolean;
begin
  Result := False;
end;

procedure TSmartyProvider.GetIndexProperties(var AMin, AMax: Integer);
begin
  AMin := 0;
  AMax := 0;
end;

function TSmartyProvider.GetVariable(AIndex: Integer; const AVarName: string): TVariableRecord;
begin
  Result :=  TVariableRecord.Null;
end;

function TSmartyProvider.GetSmartyVariable(const AVarName: string;
  AVarDetails: TVarList; var NeedFinalize: Boolean): TVariableRecord;
var
  S, VarName: string;
  VarDetails: TVarList;
  FERec: TForEachData;
  CacheRec: TCaptureCache;
begin
  Result :=  TVariableRecord.Null;
  NeedFinalize := True;

  if AVarDetails.Count = 0 then
  begin
    if CompareText(AVarName, 'now') = 0 then Result := Now
    else if CompareText(AVarName, 'ldelim') = 0 then Result := '{'
    else if CompareText(AVarName, 'rdelim') = 0 then Result := '}'
    else if CompareText(AVarName, 'templatedir') = 0 then
      Result := IncludeTrailingPathDelimiter(FEngine.TemplateFolder);
  end
  else begin
    if CompareText(AVarName, 'foreach') = 0 then
    begin
      VarDetails := AVarDetails.Clone;
      try
        if VarDetails.IsTopValueLevel(S) and FForEachList.FindRecord(S, FERec) then
        begin
          if VarDetails.CheckTopLevel(FERec.ItemVarName) then
          begin
            Result := FERec.VarData.Data[FERec.Iteration - 1].Item;
            if VarDetails.Count > 0 then
              Result := FEngine.GetVariableDetails(Result, VarDetails);
            NeedFinalize := False;
          end
          else if VarDetails.IsSimpleVariable(VarName) then
          begin
            if CompareText(VarName, 'total') = 0 then Result := FERec.Total
            else if CompareText(VarName, 'inforeach') = 0 then Result := FERec.InForEach
            else if FERec.InForEach then
              if CompareText(VarName, 'iteration') = 0 then Result := FERec.Iteration
              else if CompareText(VarName, 'start') = 0 then Result := FERec.MinIndex
              else if CompareText(VarName, 'first') = 0 then Result := FERec.First
              else if CompareText(VarName, 'last') = 0 then Result := FERec.Last
              else if CompareText(VarName, 'show') = 0 then Result := FERec.Show
              else if CompareText(VarName, FERec.KeyVarName) = 0 then
              begin
                Result := FERec.VarData.Data[FERec.Iteration - 1].Key;
                NeedFinalize := False;
              end;
          end;
        end;

      finally
        VarDetails.Finalize;
      end;
    end
    else if CompareText(AVarName, 'capture') = 0 then
    begin
      VarDetails := AVarDetails.Clone;
      try
        if VarDetails.IsTopValueLevel(S) and FindCaptureItem(S, CacheRec) then
        begin
          Result := CacheRec.VariableValue^;
          if VarDetails.Count > 0 then
            Result := FEngine.GetVariableDetails(Result, VarDetails);
          NeedFinalize := False;
        end;
      finally
        VarDetails.Finalize;
      end;
    end;
  end;

  NeedFinalize := NeedFinalize and ((Result.VarType = vtString) or (Result.VarType = vtArray));
end;

function TSmartyProvider.GetDetachVariable(const AVarName: string; AVarDetails: TVarList;
  var NeedFinalize: Boolean): TVariableRecord;
var
  FERec: TForEachData;
  VName: string;
  VList: TVarList;
begin
  if FActiveCapture.IsActive and FActiveCapture.IsItemName(AVarName) then
  begin
    Result := FActiveCapture.VarData.Data[FActiveCapture.Index].Item;
    if AVarDetails.Count > 0 then
      Result := FEngine.GetVariableDetails(Result, AVarDetails);
    NeedFinalize := False;
  end
  else if FForEachList.InForEach then
  begin
    if FForEachList.FindItemRecord(AVarName, FERec) then
    begin
      if FERec.IsNamespace then
      begin
        VList := AVarDetails.Clone;
        try
          if VList.IsTopValueLevel(VName) then
            Result := FEngine.GetVariable(FERec.Namespace, FERec.MinIndex + FERec.Iteration - 1,
              VName, VList, NeedFinalize)
          else
            Result := TVariableRecord.Null;
        finally
          VList.Finalize;
        end;
      end
      else begin
        Result := FERec.VarData.Data[FERec.Iteration - 1].Item;
        if AVarDetails.Count > 0 then
          Result := FEngine.GetVariableDetails(Result, AVarDetails);
        NeedFinalize := False;
      end;
    end
    else if FForEachList.FindKeyRecord(AVarName, FERec) and (AVarDetails.Count = 0) then
    begin
      Result := FERec.VarData.Data[FERec.Iteration - 1].Key;
      NeedFinalize := True;
    end
    else
      Result :=  TVariableRecord.Null;
  end
  else
    Result :=  TVariableRecord.Null;

  NeedFinalize := NeedFinalize and ((Result.VarType = vtString) or (Result.VarType = vtArray));
end;

{************* TVariableModifier *************}

class function TVariableModifier.CheckParams(AModifier: TVariableModifierClass;
  AParams: TStringList; AMin, AMax: Integer): Boolean;
var
  Cnt: Integer;
begin
  if Assigned(AParams) then Cnt := AParams.Count else Cnt := 0;
  Result := (AMin <= Cnt) and (Cnt <= AMax);
  if not Result then
    raise ESmartyException.CreateResFmt(@sInvalidParameters,
      [Cnt, AMin, AMax, AModifier.GetName]);
end;

class function TVariableModifier.SetParam(AParams: TStringList; AIndex: Integer;
  var Value: string): Boolean;
begin
  Result := False;
  if Assigned(AParams) then
    if AIndex  < AParams.Count then
    begin
      Value := AParams[AIndex];
      Result := True;
    end;
end;

class procedure TVariableModifier.ModifyVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
var
  I: Integer;
  ArrayData: PVariableArray;
begin
  if AVariable.IsArray then
  begin
    ArrayData := AVariable.AValue;
    if ArrayData.Count > 0 then
      for I := 0 to ArrayData.Count - 1 do
        ModifyVariable(ArrayData.Data[I].Item, AParams);
  end
  else
    ModVariable(AVariable, AParams);
end;

{************* TCapitalizeModifier *************}

class function TCapitalizeModifier.GetName: string;
begin
  Result := 'capitalize';
end;

class function TCapitalizeModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 1);
end;

class procedure TCapitalizeModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
var
  CapitalizeNumbers: Boolean;
  B: Boolean;
  S: string;
begin
  CapitalizeNumbers := False;
  if CheckParams(Self, AParams, 0, 1) and SetParam(AParams, 0, S) and TryStringToBool(S, B) then
    CapitalizeNumbers := B;

  AVariable.SetString(UCWords(AVariable.ToString, CapitalizeNumbers));
end;

{************* TCatModifier *************}

class function TCatModifier.GetName: string;
begin
  Result := 'cat';
end;

class function TCatModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 1);
end;

class procedure TCatModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
var
  Add: string;
begin
  Add := '';
  if CheckParams(Self, AParams, 0, 1) then SetParam(AParams, 0, Add);
  AVariable.SetString(AVariable.ToString + Add);
end;

{************* TTrimModifier *************}

class function TTrimModifier.GetName: string;
begin
  Result := 'trim';
end;

class function TTrimModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 1);
end;

class procedure TTrimModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
var
  Mode: string;
begin
  Mode := 'all';
  if CheckParams(Self, AParams, 0, 1) then SetParam(AParams, 0, Mode);
  if CompareText('left', Mode) = 0 then
    AVariable.SetString(SmartyTrimLeft(AVariable.ToString))
  else if CompareText('right', Mode) = 0 then
    AVariable.SetString(SmartyTrimRight(AVariable.ToString))
  else
    AVariable.SetString(SmartyTrim(AVariable.ToString));
end;

{************* TCountCharactersModifier *************}

class function TCountCharactersModifier.GetName: string;
begin
  Result := 'count_characters';
end;

class function TCountCharactersModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 1);
end;

class procedure TCountCharactersModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
var
  CountWhitespace: Boolean;
  B: Boolean;
  S: string;
begin
  CountWhitespace := False;
  if CheckParams(Self, AParams, 0, 1) and SetParam(AParams, 0, S) and TryStringToBool(S, B) then
    CountWhitespace := B;

  AVariable.SetInt(CountCharacters(AVariable.ToString, CountWhitespace));
end;

{************* TCountParagraphsModifier *************}

class function TCountParagraphsModifier.GetName: string;
begin
  Result := 'count_paragraphs';
end;

class function TCountParagraphsModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 0);
end;

class procedure TCountParagraphsModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
begin
  AVariable.SetInt(CountParagraphs(AVariable.ToString));
end;

{************* TCountWordsModifier *************}

class function TCountWordsModifier.GetName: string;
begin
  Result := 'count_words';
end;

class function TCountWordsModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 0);
end;

class procedure TCountWordsModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
begin
  AVariable.SetInt(CountWords(AVariable.ToString));
end;

{************* TDefaultModifier *************}

class function TDefaultModifier.GetName: string;
begin
  Result := 'default';
end;

class function TDefaultModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 1);
end;

class procedure TDefaultModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
var
  DefValue: string;
begin
  DefValue := '';
  if CheckParams(Self, AParams, 0, 1) then SetParam(AParams, 0, DefValue);

  case AVariable.VarType of
    vtNull:
      AVariable.SetString(DefValue);
    vtString:
      if string(AVariable.SValue) = '' then AVariable.SetString(DefValue);
  end;
end;

{************* THTMLEncodeModifier *************}

class function THTMLEncodeModifier.GetName: string;
begin
  Result := 'html_encode';
end;

class function THTMLEncodeModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 0);
end;

class procedure THTMLEncodeModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
begin
  AVariable.SetString(HTMLEncode(AVariable.ToString));
end;

{************* THTMLEncodeAllModifier *************}

class function THTMLEncodeAllModifier.GetName: string;
begin
  Result := 'html_encode_all';
end;

class function THTMLEncodeAllModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 0);
end;

class procedure THTMLEncodeAllModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
begin
  AVariable.SetString(HTMLEncodeEntities(AVariable.ToString));
end;

{************* TXMLEncodeModifier *************}

class function TXMLEncodeModifier.GetName: string;
begin
  Result := 'xml_encode';
end;

class function TXMLEncodeModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 0);
end;

class procedure TXMLEncodeModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
begin
  AVariable.SetString(XMLEncode(AVariable.ToString));
end;

{************* TFileEncodeModifier *************}

class function TFileEncodeModifier.GetName: string;
begin
  Result := 'file_encode';
end;

class function TFileEncodeModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 0);
end;

class procedure TFileEncodeModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
begin
  AVariable.SetString(FileEncode(AVariable.ToString));
end;

{************* TDateFormatModifier *************}

class function TDateFormatModifier.GetName: string;
begin
  Result := 'date_format';
end;

class function TDateFormatModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 1);
end;

class procedure TDateFormatModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
var
  S, DateFormat: string;
  DT: TDateTime;
begin
  DateFormat := '';
  if CheckParams(Self, AParams, 0, 1) then SetParam(AParams, 0, DateFormat);

  if CompareText(DateFormat, 'shortdate') = 0 then DateFormat := FormatSettings.ShortDateFormat
  else if CompareText(DateFormat, 'longdate') = 0 then DateFormat := FormatSettings.LongDateFormat
  else if CompareText(DateFormat, 'shorttime') = 0 then DateFormat := FormatSettings.ShortTimeFormat
  else if CompareText(DateFormat, 'longtime') = 0 then DateFormat := FormatSettings.LongTimeFormat;

  case AVariable.VarType of
    vtNull, vtBoolean:
    begin
      AVariable.SetNull;
      Exit;
    end;
    vtInt:
    begin
      DT := AVariable.IValue;
      if DateFormat = '' then DateFormat := FormatSettings.ShortDateFormat;
    end;
    vtFloat: 
      DT := AVariable.FValue;
    vtDateStrict:
    begin
      DT := AVariable.DSValue;
      if DateFormat = '' then DateFormat := FormatSettings.ShortDateFormat;
    end;
    vtDateLoose:
    begin
      DT := DateTimeFromRecord(AVariable.DLValue);
      if DateFormat = '' then DateFormat := FormatSettings.ShortDateFormat;
    end;
    vtDateTime:
      DT := AVariable.DTValue;
    vtString:
      if not TryStrToDate(string(AVariable.SValue), DT) or
        not TryStrToTime(string(AVariable.SValue), DT) or
        not TryStrToDateTime(string(AVariable.SValue), DT) then
      begin
        AVariable.SetNull;
        Exit;
      end;
  end;

  DateTimeToString(S, DateFormat, DT);
  AVariable.SetString(S);
end;


{************* TFloatFormatModifier *************}

class function TFloatFormatModifier.GetName: string;
begin
  Result := 'float_format';
end;

class function TFloatFormatModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 1);
end;

class procedure TFloatFormatModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
var
  Format: string;
begin
  Format := '';
  if CheckParams(Self, AParams, 0, 1) then SetParam(AParams, 0, Format);

  AVariable.SetString(FormatFloat(Format, AVariable.ToFloat));
end;

{************* TLowerModifier *************}

class function TLowerModifier.GetName: string;
begin
  Result := 'lower';
end;

class function TLowerModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 0);
end;

class procedure TLowerModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
begin
  AVariable.SetString(AnsiLowerCase(AVariable.ToString));
end;

{************* TUpperModifier *************}

class function TUpperModifier.GetName: string;
begin
  Result := 'upper';
end;

class function TUpperModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 0);
end;

class procedure TUpperModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
begin
  AVariable.SetString(AnsiUpperCase(AVariable.ToString));
end;

{************* TNl2BrModifier *************}

class function TNl2BrModifier.GetName: string;
begin
  Result := 'nl2br';
end;

class function TNl2BrModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 0);
end;

class procedure TNl2BrModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
var
  S: string;
begin
  S := AVariable.ToString;
  S := StringReplace(S, sLineBreak, '<br/>', [rfReplaceAll, rfIgnoreCase]);
  S := StringReplace(S, #13, '<br/>', [rfReplaceAll, rfIgnoreCase]);
  S := StringReplace(S, #10, '<br/>', [rfReplaceAll, rfIgnoreCase]);
  AVariable.SetString(S);
end;

{************* TTruncateModifier *************}

class function TTruncateModifier.GetName: string;
begin
  Result := 'truncate';
end;

class function TTruncateModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 4);
end;

class procedure TTruncateModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
var
  Length, I: Integer;
  S, Etc: string;
  BreakWords, Middle, B: Boolean;
begin
  Length := 80;
  Etc := '...';
  BreakWords := False;
  Middle := False;

  if CheckParams(Self, AParams, 0, 4) then
  begin
    if SetParam(AParams, 0, S) and TryStrToInt(S, I) and (I > 0) then Length := I;
    SetParam(AParams, 1, Etc);
    if SetParam(AParams, 2, S) and TryStringToBool(S, B) then BreakWords := B;
    if SetParam(AParams, 3, S) and TryStringToBool(S, B) then Middle := B;
  end;

  AVariable.SetString(TruncateString(AVariable.ToString, Length, Etc, BreakWords, Middle));
end;

{************* TStripModifier *************}

class function TStripModifier.GetName: string;
begin
  Result := 'strip';
end;

class function TStripModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 1);
end;

class procedure TStripModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
var
  StripStr: string;
begin
  StripStr := ' ';
  if CheckParams(Self, AParams, 0, 1) then SetParam(AParams, 0, StripStr);

  AVariable.SetString(Strip(AVariable.ToString, StripStr));
end;

{************* TSpacifyModifier *************}

class function TSpacifyModifier.GetName: string;
begin
  Result := 'spacify';
end;

class function TSpacifyModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 1);
end;

class procedure TSpacifyModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
var
  StripStr: string;
begin
  StripStr := ' ';
  if CheckParams(Self, AParams, 0, 1) then SetParam(AParams, 0, StripStr);

  AVariable.SetString(Spacify(AVariable.ToString, StripStr));
end;

{************* TWordwrapModifier *************}

class function TWordwrapModifier.GetName: string;
begin
  Result := 'wordwrap';
end;

class function TWordwrapModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 2);
end;

class procedure TWordwrapModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
var
  MaxCol, I: Integer;
  S, BreakStr: string;
begin
  MaxCol := 80;
  BreakStr := sLineBreak;

  if CheckParams(Self, AParams, 0, 2) then
  begin
    if SetParam(AParams, 0, S) and TryStrToInt(S, I) and (I > 0) then MaxCol := I;
    SetParam(AParams, 1, BreakStr);
  end;

  AVariable.SetString(Wordwrap(AVariable.ToString, MaxCol, BreakStr));
end;

{************* TIndentModifier *************}

class function TIndentModifier.GetName: string;
begin
  Result := 'indent';
end;

class function TIndentModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 2);
end;

class procedure TIndentModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
var
  IndentCount, I: Integer;
  S, IndentStr: string;
begin
  IndentCount := 4;
  IndentStr := ' ';

  if CheckParams(Self, AParams, 0, 2) then
  begin
    if SetParam(AParams, 0, S) and TryStrToInt(S, I) and (I > 0) then IndentCount := I;
    SetParam(AParams, 1, IndentStr);
  end;

  S := '';
  for I := 1 to IndentCount do S := S + IndentStr;

  AVariable.SetString(IndentString(AVariable.ToString, S));
end;

{************* TReplaceModifier *************}

class function TReplaceModifier.GetName: string;
begin
  Result := 'replace';
end;

class function TReplaceModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 2, 3);
end;

class procedure TReplaceModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
var
  ReplaceFrom, ReplaceTo, S: string;
  B, CaseSensitive: Boolean;
begin
  ReplaceFrom := '';
  ReplaceTo := '';
  CaseSensitive := False;

  if CheckParams(Self, AParams, 2, 3) then
  begin
    SetParam(AParams, 0, ReplaceFrom);
    SetParam(AParams, 1, ReplaceTo);
    if SetParam(AParams, 2, S) and TryStringToBool(S, B) then CaseSensitive := B;
  end;

  if CaseSensitive then
    AVariable.SetString(AnsiReplaceStr(AVariable.ToString, ReplaceFrom, ReplaceTo))
  else
    AVariable.SetString(AnsiReplaceText(AVariable.ToString, ReplaceFrom, ReplaceTo));
end;

{************* TStripTagsModifier *************}

class function TStripTagsModifier.GetName: string;
begin
  Result := 'strip_tags';
end;

class function TStripTagsModifier.CheckInputParams(AParams: TStringList): Boolean;
begin
  Result := CheckParams(Self, AParams, 0, 2);
end;

class procedure TStripTagsModifier.ModVariable(const AVariable: TVariableRecord;
  AParams: TStringList);
var
  NoSpace, ParseTags, B: Boolean;
  S: string;
begin
  NoSpace := False;
  ParseTags := False;

  if CheckParams(Self, AParams, 0, 4) then
  begin
    if SetParam(AParams, 0, S) and TryStringToBool(S, B) then NoSpace := B;
    if SetParam(AParams, 1, S) and TryStringToBool(S, B) then ParseTags := B;
  end;

  AVariable.SetString(StripTags(AVariable.ToString, NoSpace, ParseTags));
end;

{************* TSmartyFunction *************}

class function TSmartyFunction.IsParam(Index: Integer;
  const AParams: array of TVariableRecord; var Param: TVariableRecord): Boolean;
begin
  Result := (Index >= 0) and (Index < Length(AParams));
  if Result then Param := AParams[Index];
end;

class function TSmartyFunction.GetParam(Index: Integer;
  const AParams: array of TVariableRecord): TVariableRecord;
begin
  if (Index >= 0) and (Index < Length(AParams)) then
    Result := AParams[Index]
  else
    Result := TVariableRecord.Null;
end;

class function TSmartyFunction.EvaluateFunction(const AParams: array of TVariableRecord): TVariableRecord;
begin
  if CheckParams(Length(AParams)) then
    Result := Evaluate(AParams)
  else
    Result := TVariableRecord.Null;
end;

{************* TIsNullFunction *************}

class function TIsNullFunction.GetName: string;
begin
  Result := 'is_null';
end;

class function TIsNullFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TIsNullFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := GetParam(0, AParams).IsNull;
end;

{************* TIsEmptyFunction *************}

class function TIsEmptyFunction.GetName: string;
begin
  Result := 'is_empty';
end;

class function TIsEmptyFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TIsEmptyFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := GetParam(0, AParams).IsEmpty;
end;

{************* TIsBooleanFunction *************}

class function TIsBooleanFunction.GetName: string;
begin
  Result := 'is_bool';
end;

class function TIsBooleanFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TIsBooleanFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := GetParam(0, AParams).IsBoolean;
end;

{************* TIsIntegerFunction *************}

class function TIsIntegerFunction.GetName: string;
begin
  Result := 'is_int';
end;

class function TIsIntegerFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TIsIntegerFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := GetParam(0, AParams).IsInt;
end;

{************* TIsFloatFunction *************}

class function TIsFloatFunction.GetName: string;
begin
  Result := 'is_float';
end;

class function TIsFloatFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TIsFloatFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := GetParam(0, AParams).IsFloat;
end;

{************* TIsNumberFunction *************}

class function TIsNumberFunction.GetName: string;
begin
  Result := 'is_number';
end;

class function TIsNumberFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TIsNumberFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := GetParam(0, AParams).IsNumber;
end;

{************* TIsDateStrictFunction *************}

class function TIsDateStrictFunction.GetName: string;
begin
  Result := 'is_datestrict';
end;

class function TIsDateStrictFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TIsDateStrictFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := GetParam(0, AParams).IsDateStrict;
end;

{************* TIsDateLooseFunction *************}

class function TIsDateLooseFunction.GetName: string;
begin
  Result := 'is_dateloose';
end;

class function TIsDateLooseFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TIsDateLooseFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := GetParam(0, AParams).IsDateLoose;
end;

{************* TIsDateTimeFunction *************}

class function TIsDateTimeFunction.GetName: string;
begin
  Result := 'is_datetime';
end;

class function TIsDateTimeFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TIsDateTimeFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := GetParam(0, AParams).IsDateTime;
end;

{************* TIsDateFunction *************}

class function TIsDateFunction.GetName: string;
begin
  Result := 'is_date';
end;

class function TIsDateFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TIsDateFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := GetParam(0, AParams).IsDate;
end;

{************* TIsStringFunction *************}

class function TIsStringFunction.GetName: string;
begin
  Result := 'is_string';
end;

class function TIsStringFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TIsStringFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := GetParam(0, AParams).IsString;
end;

{************* TIsArrayFunction *************}

class function TIsArrayFunction.GetName: string;
begin
  Result := 'is_array';
end;

class function TIsArrayFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TIsArrayFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := GetParam(0, AParams).IsArray;
end;

{************* TArrayLengthFunction *************}

class function TArrayLengthFunction.GetName: string;
begin
  Result := 'array_length';
end;

class function TArrayLengthFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TArrayLengthFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  V: TVariableRecord;
begin
  V := GetParam(0, AParams);
  case V.VarType of
    vtArray:
      Result := PVariableArray(V.AValue).Count;
  else
    Result := 0;
  end;
end;

{************* TArrayIndexFunction *************}

class function TArrayIndexFunction.GetName: string;
begin
  Result := 'array_index';
end;

class function TArrayIndexFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 2);
end;

class function TArrayIndexFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  V1, V2: TVariableRecord;
  I: Integer;
begin
  V1 := GetParam(0, AParams);
  V2 := GetParam(1, AParams);
  case V1.VarType of
    vtArray:
    begin
      I := V2.ToInt;
      if (I >= 0) and (I < PVariableArray(V1.AValue).Count) then
        Result := PVariableArray(V1.AValue).Data[I].Item.Clone
      else
        Result := TVariableRecord.Null;
    end;
  else
    Result := TVariableRecord.Null;
  end;
end;

{************* TArrayKeyFunction *************}

class function TArrayKeyFunction.GetName: string;
begin
  Result := 'array_key';
end;

class function TArrayKeyFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 2);
end;

class function TArrayKeyFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  V1, V2: TVariableRecord;
  I: Integer;
  S: string;
begin
  V1 := GetParam(0, AParams);
  V2 := GetParam(1, AParams);
  case V1.VarType of
    vtArray:
    begin
      S := V2.ToString;
      if S <> '' then
      begin
        for I := 0 to PVariableArray(V1.AValue).Count - 1 do
          if CompareText(PVariableArray(V1.AValue).Data[I].Key, S) = 0 then
          begin
            Result := PVariableArray(V1.AValue).Data[I].Item.Clone;
            Break;
          end;
      end
      else
        Result := TVariableRecord.Null;
    end;
  else
    Result := TVariableRecord.Null;
  end;
end;

{************* TCountFunction *************}

class function TCountFunction.GetName: string;
begin
  Result := 'count';
end;

class function TCountFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TCountFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  V: TVariableRecord;
begin
  V := GetParam(0, AParams);
  case V.VarType of
    vtArray:
      Result := PVariableArray(V.AValue).Count;
  else
    Result := 0;
  end;
end;

{************* TEchoFunction *************}

class function TEchoFunction.GetName: string;
begin
  Result := 'echo';
end;

class function TEchoFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TEchoFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := GetParam(0, AParams).Clone;
end;

{************* TPrintFunction *************}

class function TPrintFunction.GetName: string;
begin
  Result := 'print';
end;

class function TPrintFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount >= 0);
end;

class function TPrintFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  I: Integer;
  S: string;
begin
  S := '';
  for I := 0 to High(AParams) do S := S + AParams[I].ToString;
  Result := S;
end;

{************* THTMLEncodeFunction *************}

class function THTMLEncodeFunction.GetName: string;
begin
  Result := 'html_encode';
end;

class function THTMLEncodeFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function THTMLEncodeFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := HTMLEncode(GetParam(0, AParams).ToString);
end;

{************* THTMLEncodeAllFunction *************}

class function THTMLEncodeAllFunction.GetName: string;
begin
  Result := 'html_encode_all';
end;

class function THTMLEncodeAllFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function THTMLEncodeAllFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := HTMLEncodeEntities(GetParam(0, AParams).ToString);
end;

{************* TXMLEncodeFunction *************}

class function TXMLEncodeFunction.GetName: string;
begin
  Result := 'xml_encode';
end;

class function TXMLEncodeFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TXMLEncodeFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := XMLEncode(GetParam(0, AParams).ToString);
end;

{************* TFileEncodeFunction *************}

class function TFileEncodeFunction.GetName: string;
begin
  Result := 'file_encode';
end;

class function TFileEncodeFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TFileEncodeFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := FileEncode(GetParam(0, AParams).ToString);
end;

{************* TTrimFunction *************}

class function TTrimFunction.GetName: string;
begin
  Result := 'trim';
end;

class function TTrimFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount >= 1) and (AParamsCount <= 2);
end;

class function TTrimFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  S, Mode: string;
  VR: TVariableRecord;
begin
  S := GetParam(0, AParams).ToString;
  VR := GetParam(1, AParams);
  if VR.IsNull then
    Result := SmartyTrim(S)
  else begin
    Mode := VR.ToString;
    if CompareText(Mode, 'left') = 0 then
      Result := SmartyTrimLeft(S)
    else if CompareText(Mode, 'right') = 0 then
      Result := SmartyTrimRight(S)
    else
      Result := SmartyTrim(S);
  end;
end;

{************* TTruncateFunction *************}

class function TTruncateFunction.GetName: string;
begin
  Result := 'truncate';
end;

class function TTruncateFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount >= 1) and (AParamsCount <= 5);
end;

class function TTruncateFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  S, Etc: string;
  L: Integer;
  VR: TVariableRecord;
  BreakWords, Middle: Boolean;
begin
  S := GetParam(0, AParams).ToString;
  VR := GetParam(1, AParams);
  if VR.IsNull then L := 80 else L := VR.ToInt;
  VR := GetParam(2, AParams);
  if VR.IsNull then Etc := '...' else Etc := VR.ToString;
  VR := GetParam(3, AParams);
  if VR.IsNull then BreakWords := False else BreakWords := VR.ToBool;
  VR := GetParam(4, AParams);
  if VR.IsNull then Middle := False else Middle := VR.ToBool;

  Result := TruncateString(S, L, Etc, BreakWords, Middle);
end;

{************* TStripFunction *************}

class function TStripFunction.GetName: string;
begin
  Result := 'strip';
end;

class function TStripFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount >= 1) and (AParamsCount <= 2);
end;

class function TStripFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  StripStr: string;
  VR: TVariableRecord;
begin
  VR := GetParam(1, AParams);
  if VR.IsNull then
    StripStr := ' '
  else
    StripStr := VR.ToString;

  Result := Strip(GetParam(0, AParams).ToString, StripStr);
end;

{************* TStripTagsFunction *************}

class function TStripTagsFunction.GetName: string;
begin
  Result := 'strip_tags';
end;

class function TStripTagsFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount >= 1) and (AParamsCount <= 3);
end;

class function TStripTagsFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  VR: TVariableRecord;
  NoSpace, ParseTags: Boolean;
begin
  VR := GetParam(1, AParams);
  if VR.IsNull then NoSpace := True else NoSpace := VR.ToBool;
  VR := GetParam(2, AParams);
  if VR.IsNull then ParseTags := True else ParseTags := VR.ToBool;

  Result := StripTags(GetParam(0, AParams).ToString, NoSpace, ParseTags);
end;

{************* TSpacifyFunction *************}

class function TSpacifyFunction.GetName: string;
begin
  Result := 'spacify';
end;

class function TSpacifyFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount >= 1) and (AParamsCount <= 2);
end;

class function TSpacifyFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  StripStr: string;
  VR: TVariableRecord;
begin
  VR := GetParam(1, AParams);
  if VR.IsNull then
    StripStr := ' '
  else
    StripStr := VR.ToString;

  Result := Spacify(GetParam(0, AParams).ToString, StripStr);
end;

{************* TWordwrapFunction *************}

class function TWordwrapFunction.GetName: string;
begin
  Result := 'wordwrap';
end;

class function TWordwrapFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount >= 1) and (AParamsCount <= 3);
end;

class function TWordwrapFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  MaxCol: Integer;
  BreakStr: string;
  VR: TVariableRecord;
begin
  VR := GetParam(1, AParams);
  if VR.IsNull then
    MaxCol := 80
  else
    MaxCol := VR.ToInt;

  VR := GetParam(2, AParams);
  if VR.IsNull then
    BreakStr := sLineBreak
  else
    BreakStr := VR.ToString;


  Result := Wordwrap(GetParam(0, AParams).ToString, MaxCol, BreakStr);
end;

{************* TIndentFunction *************}

class function TIndentFunction.GetName: string;
begin
  Result := 'indent';
end;

class function TIndentFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount >= 1) and (AParamsCount <= 3);
end;

class function TIndentFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  I, IndentCount: Integer;
  S, IndentStr: string;
  VR: TVariableRecord;
begin
  VR := GetParam(1, AParams);
  if VR.IsNull then
    IndentCount := 80
  else
    IndentCount := VR.ToInt;

  VR := GetParam(2, AParams);
  if VR.IsNull then
    IndentStr := sLineBreak
  else
    IndentStr := VR.ToString;

  S := '';
  if IndentCount >= 1 then
    for I := 1 to IndentCount do S := S + IndentStr;

  Result := IndentString(GetParam(0, AParams).ToString, S);
end;

{************* TCapitalizeFunction *************}

class function TCapitalizeFunction.GetName: string;
begin
  Result := 'capitalize';
end;

class function TCapitalizeFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount >= 1) and (AParamsCount <= 2);
end;

class function TCapitalizeFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  VR: TVariableRecord;
  Digits: Boolean;
begin
  VR := GetParam(1, AParams);
  if VR.IsNull then Digits := False else Digits := VR.ToBool;
  Result := UCWords(GetParam(0, AParams).ToString, Digits);
end;

{************* TCountCharactersFunction *************}

class function TCountCharactersFunction.GetName: string;
begin
  Result := 'count_characters';
end;

class function TCountCharactersFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount >= 1) and (AParamsCount <= 2);
end;

class function TCountCharactersFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  VR: TVariableRecord;
  CountWhitespace: Boolean;
begin
  VR := GetParam(1, AParams);
  if VR.IsNull then CountWhitespace := False else CountWhitespace := VR.ToBool;
  Result := CountCharacters(GetParam(0, AParams).ToString, CountWhitespace);
end;

{************* TCountWordsFunction *************}

class function TCountWordsFunction.GetName: string;
begin
  Result := 'count_words';
end;

class function TCountWordsFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TCountWordsFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := CountWords(GetParam(0, AParams).ToString);
end;

{************* TCountParagraphsFunction *************}

class function TCountParagraphsFunction.GetName: string;
begin
  Result := 'count_paragraphs';
end;

class function TCountParagraphsFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TCountParagraphsFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := CountParagraphs(GetParam(0, AParams).ToString);
end;

{************* TUpperCaseFunction *************}

class function TUpperCaseFunction.GetName: string;
begin
  Result := 'upper_case';
end;

class function TUpperCaseFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TUpperCaseFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := AnsiUpperCase(GetParam(0, AParams).ToString);
end;

{************* TLowerCaseFunction *************}

class function TLowerCaseFunction.GetName: string;
begin
  Result := 'lower_case';
end;

class function TLowerCaseFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TLowerCaseFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := AnsiLowerCase(GetParam(0, AParams).ToString);
end;

{************* TResemblesFunction *************}

class function TResemblesFunction.GetName: string;
begin
  Result := 'resembles';
end;

class function TResemblesFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 2);
end;

class function TResemblesFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  Result := AnsiResemblesText(GetParam(0, AParams).ToString, GetParam(1, AParams).ToString);
end;

{************* TContainsFunction *************}

class function TContainsFunction.GetName: string;
begin
  Result := 'contains';
end;

class function TContainsFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount >= 2) and (AParamsCount <= 3);
end;

class function TContainsFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  VR: TVariableRecord;
  CaseSensitive: Boolean;
begin
  VR := GetParam(2, AParams);
  if VR.IsNull then CaseSensitive := False else CaseSensitive := VR.ToBool;

  if CaseSensitive then
    Result := AnsiContainsStr(GetParam(0, AParams).ToString, GetParam(1, AParams).ToString)
  else
    Result := AnsiContainsText(GetParam(0, AParams).ToString, GetParam(1, AParams).ToString);
end;

{************* TStartsFunction *************}

class function TStartsFunction.GetName: string;
begin
  Result := 'stars';
end;

class function TStartsFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount >= 2) and (AParamsCount <= 3);
end;

class function TStartsFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  VR: TVariableRecord;
  CaseSensitive: Boolean;
begin
  VR := GetParam(2, AParams);
  if VR.IsNull then CaseSensitive := False else CaseSensitive := VR.ToBool;

  if CaseSensitive then
    Result := AnsiStartsStr(GetParam(1, AParams).ToString, GetParam(0, AParams).ToString)
  else
    Result := AnsiStartsText(GetParam(1, AParams).ToString, GetParam(0, AParams).ToString);
end;

{************* TEndsFunction *************}

class function TEndsFunction.GetName: string;
begin
  Result := 'ends';
end;

class function TEndsFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount >= 2) and (AParamsCount <= 3);
end;

class function TEndsFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  VR: TVariableRecord;
  CaseSensitive: Boolean;
begin
  VR := GetParam(2, AParams);
  if VR.IsNull then CaseSensitive := False else CaseSensitive := VR.ToBool;

  if CaseSensitive then
    Result := AnsiEndsStr(GetParam(1, AParams).ToString, GetParam(0, AParams).ToString)
  else
    Result := AnsiEndsText(GetParam(1, AParams).ToString, GetParam(0, AParams).ToString);
end;

{************* TReplaceFunction *************}

class function TReplaceFunction.GetName: string;
begin
  Result := 'replace';
end;

class function TReplaceFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount >= 3) and (AParamsCount <= 4);
end;

class function TReplaceFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  VR: TVariableRecord;
  CaseSensitive: Boolean;
begin
  VR := GetParam(3, AParams);
  if VR.IsNull then CaseSensitive := False else CaseSensitive := VR.ToBool;

  if CaseSensitive then
    Result := AnsiReplaceStr(GetParam(0, AParams).ToString,
      GetParam(1, AParams).ToString,
      GetParam(2, AParams).ToString)
  else
    Result := AnsiReplaceText(GetParam(0, AParams).ToString,
      GetParam(1, AParams).ToString,
      GetParam(2, AParams).ToString);
end;

{************* TFloatFormatFunction *************}

class function TFloatFormatFunction.GetName: string;
begin
  Result := 'float_format';
end;

class function TFloatFormatFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount >= 1) and (AParamsCount <= 2);
end;

class function TFloatFormatFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  V: TVariableRecord;
  Format: string;
begin
  V := GetParam(1, AParams);
  if V.VarType = vtNull then
    Format := ''
  else
    Format := V.ToString;

  Result := FormatFloat(Format, GetParam(0, AParams).ToFloat);
end;

{************* TIfThenFunction *************}

class function TIfThenFunction.GetName: string;
begin
  Result := 'ifthen';
end;

class function TIfThenFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 3);
end;

class function TIfThenFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
begin
  if GetParam(0, AParams).ToBool then
    Result := GetParam(1, AParams).Clone
  else
    Result := GetParam(2, AParams).Clone;
end;

{************* TDateFormatFunction *************}

class function TDateFormatFunction.GetName: string;
begin
  Result := 'date_format';
end;

class function TDateFormatFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount >= 1) and (AParamsCount <= 2);
end;

class function TDateFormatFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  V: TVariableRecord;
  DT: TDateTime;
  S, DateFormat: string;
begin
  V := GetParam(1, AParams);
  if V.IsNull then
    DateFormat := ''
  else
    DateFormat := V.ToString;

  if CompareText(DateFormat, 'shortdate') = 0 then DateFormat := FormatSettings.ShortDateFormat
  else if CompareText(DateFormat, 'longdate') = 0 then DateFormat := FormatSettings.LongDateFormat
  else if CompareText(DateFormat, 'shorttime') = 0 then DateFormat := FormatSettings.ShortTimeFormat
  else if CompareText(DateFormat, 'longtime') = 0 then DateFormat := FormatSettings.LongTimeFormat;

  V := GetParam(0, AParams);

  case V.VarType of
    vtNull, vtBoolean:
      Exit(TVariableRecord.Null);
    vtInt:
    begin
      DT := V.IValue;
      if DateFormat = '' then DateFormat := FormatSettings.ShortDateFormat;
    end;
    vtFloat:
      DT := V.FValue;
    vtDateStrict:
    begin
      DT := V.DSValue;
      if DateFormat = '' then DateFormat := FormatSettings.ShortDateFormat;
    end;
    vtDateLoose:
    begin
      DT := DateTimeFromRecord(V.DLValue);
      if DateFormat = '' then DateFormat := FormatSettings.ShortDateFormat;
    end;
    vtDateTime:
      DT := V.DTValue;
    vtString:
      if not TryStrToDate(string(V.SValue), DT) or
        not TryStrToTime(string(V.SValue), DT) or
        not TryStrToDateTime(string(V.SValue), DT) then
        Exit(TVariableRecord.Null);
  end;

  DateTimeToString(S, DateFormat, DT);
  Result := S;
end;

{************* TFullYearsFunction *************}

class function TFullYearsFunction.GetName: string;
begin
  Result := 'full_years';
end;

class function TFullYearsFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount >= 1) and (AParamsCount <= 2);
end;

class function TFullYearsFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  V1, V2: TVariableRecord;
  DLFrom, DLTo: TDateRecord;
  DT: TDateTime;
  Years: Integer;
begin
  V1 := GetParam(0, AParams);
  case V1.VarType of
    vtInt:
      DLFrom := DateTimeToRecord(V1.IValue);
    vtFloat:
      DLFrom := DateTimeToRecord(V1.FValue);
    vtDateStrict:
      DLFrom := DateTimeToRecord(V1.DSValue);
    vtDateLoose:
      DLFrom := V1.DLValue;
    vtDateTime:
      DLFrom := DateTimeToRecord(V1.DTValue);
    vtString:
      if TryStrToDateTime(string(V1.SValue), DT) then
        DLFrom := DateTimeToRecord(DT)
      else
        Exit(TVariableRecord.Null);
  else
    Exit(TVariableRecord.Null);
  end;

  V2 := GetParam(1, AParams);
  case V2.VarType of
    vtInt:
      DLTo := DateTimeToRecord(V2.IValue);
    vtFloat:
      DLTo := DateTimeToRecord(V2.FValue);
    vtDateStrict:
      DLTo := DateTimeToRecord(V2.DSValue);
    vtDateLoose:
      DLTo := V2.DLValue;
    vtDateTime:
      DLTo := DateTimeToRecord(V2.DTValue);
    vtString:
      if TryStrToDateTime(string(V2.SValue), DT) then
        DLTo := DateTimeToRecord(DT)
      else
        DLTo := DateTimeToRecord(Now);
  else
    DLTo := DateTimeToRecord(Now);
  end;

  if (DLFrom.Year <> 0) and (DLTo.Year <> 0) then
  begin
    if (DLFrom.Month = 0) or (DLTo.Month = 0) then
      Years := 0
    else begin
      if DLTo.Month > DLFrom.Month then
        Years := 0
      else if DLTo.Month < DLFrom.Month then
        Years := -1
      else begin
        // DLTo.Month = DLFrom.Month
        if (DLFrom.Day = 0) or (DLTo.Day = 0) then
          Years := 0
        else begin
          if DLTo.Day >= DLFrom.Day then
            Years := 0
          else
            Years := -1
        end;
      end;
    end;

    Years := DLTo.Year - DLFrom.Year + Years;
    if Years >= 0 then
      Result := Years
    else
      Result := TVariableRecord.Null;
  end
  else
    Result := TVariableRecord.Null;
end;

{************* TYearOfFunction *************}

class function TYearOfFunction.GetName: string;
begin
  Result := 'year_of';
end;

class function TYearOfFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TYearOfFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  V: TVariableRecord;
  DT: TDateTime;
begin
  V := GetParam(0, AParams);
  case V.VarType of
    vtBoolean:
      if V.BValue then
        Result := YearOf(Now)
      else
        Result := 0;
    vtInt:
      Result := YearOf(V.IValue);
    vtFloat:
      Result := YearOf(V.FValue);
    vtDateStrict:
      Result := YearOf(V.DSValue);
    vtDateLoose:
      Result := V.DLValue.Year;
    vtDateTime:
      Result := YearOf(V.DTValue);
    vtString:
      if TryStrToDateTime(string(V.SValue), DT) then
        Result := YearOf(DT)
      else
        Result := 0;
    vtArray:
      Result := 0;
  else
    Result := 0;
  end;
end;

{************* TMonthOfFunction *************}

class function TMonthOfFunction.GetName: string;
begin
  Result := 'month_of';
end;

class function TMonthOfFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TMonthOfFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  V: TVariableRecord;
  DT: TDateTime;
begin
  V := GetParam(0, AParams);
  case V.VarType of
    vtBoolean:
      if V.BValue then
        Result := MonthOf(Now)
      else
        Result := 0;
    vtInt:
      Result := MonthOf(V.IValue);
    vtFloat:
      Result := MonthOf(V.FValue);
    vtDateStrict:
      Result := MonthOf(V.DSValue);
    vtDateLoose:
      Result := V.DLValue.Month;
    vtDateTime:
      Result := MonthOf(V.DTValue);
    vtString:
      if TryStrToDateTime(string(V.SValue), DT) then
        Result := MonthOf(DT)
      else
        Result := 0;
    vtArray:
      Result := 0;
  else
    Result := 0;
  end;
end;

{************* TDayOfFunction *************}

class function TDayOfFunction.GetName: string;
begin
  Result := 'day_of';
end;

class function TDayOfFunction.CheckParams(AParamsCount: Integer): Boolean;
begin
  Result := (AParamsCount = 1);
end;

class function TDayOfFunction.Evaluate(const AParams: array of TVariableRecord): TVariableRecord;
var
  V: TVariableRecord;
  DT: TDateTime;
begin
  V := GetParam(0, AParams);
  case V.VarType of
    vtBoolean:
      if V.BValue then
        Result := DayOf(Now)
      else
        Result := 0;
    vtInt:
      Result := DayOf(V.IValue);
    vtFloat:
      Result := DayOf(V.FValue);
    vtDateStrict:
      Result := DayOf(V.DSValue);
    vtDateLoose:
      Result := V.DLValue.Day;
    vtDateTime:
      Result := DayOf(V.DTValue);
    vtString:
      if TryStrToDateTime(string(V.SValue), DT) then
        Result := DayOf(DT)
      else
        Result := 0;
    vtArray:
      Result := 0;
  else
    Result := 0;
  end;
end;


{************* TTemplateActions *************}

function TTemplateActions.Execute: string;
var
  I: Integer;
begin
  Result := '';
  if Count > 0 then
    for I := 0 to Count - 1 do
      Result := Result + Items[I].Execute;
end;

{************* TTemplateAction *************}

constructor TTemplateAction.Create(AEngine: TSmartyEngine);
begin
  inherited Create;
  FEngine := AEngine;
end;

class function TTemplateAction.IsComment(var ACommand: string): Boolean;
begin
  if (ACommand[1] = '*') and (ACommand[Length(ACommand)] = '*') then
    Result := True
  else begin
    Result := False;
    ACommand := SmartyTrim(ACommand);
  end;
end;

class function TTemplateAction.IsTag(const ATag: string; const ACommand: string;
   AOnlyTag: Boolean = False): Boolean;
begin
  if AOnlyTag then
    Result := CompareText(ATag, ACommand) = 0
  else
    Result := StartsWithSpace(ATag, ACommand) or (CompareText(ATag, ACommand) = 0);
end;

class function TTemplateAction.IsTagAndGetCommand(const ATag: string;
  var ACommand: string): Boolean;
begin
  if CompareText(ATag, ACommand) = 0 then
  begin
    Result := True;
    ACommand := '';
  end
  else if StartsWithSpace(ATag, ACommand) then
  begin
    Result := True;
    Delete(ACommand, 1, Length(ATag) + 1);
  end
  else
    Result := False;
end;

class function TTemplateAction.IsExitCommand(const ACommand: string;
  ABreakAction: TNestAction): Boolean;
begin
  //IF Tags
  if IsTag('/if', ACommand, True) or
    IsTag('else', ACommand, True) or IsTag('elseif', ACommand) or
    IsTag('elseifdef', ACommand) or IsTag('elseifndef', ACommand) or
    IsTag('elseifempty', ACommand) or IsTag('elseifnempty', ACommand) then
  begin
    Result := True;
    if ABreakAction <> naIf then
      raise ESmartyException.CreateResFmt(@sInvalidIfCommand, [ACommand]);
  end
  //ForEach Tags
  else if IsTag('/foreach', ACommand, True) or IsTag('foreachelse', ACommand, True) then
  begin
    Result := True;
    if ABreakAction <> naForEach then
      raise ESmartyException.CreateResFmt(@sInvalidForEachCommand, [ACommand]);
  end
  else
    Result := False;
end;

class function TTemplateAction.ParseFunction(const ACommand: string): TStringList;
var
  I: Integer;
  InQuotes: Boolean;
  Command, Attribute: string;
  Ch: Char;
begin
  Result := TStringList.Create;
  InQuotes := False;
  Command := SmartyTrim(ACommand);
  I := 1;
  Attribute := '';

  while I <= Length(Command) do
  begin
    Ch := Command[I];
    Inc(I);

    if Ch = '"' then
      if InQuotes then
      begin
        if not (GetChar(Command, I) = '"') then
          InQuotes := False
        else
          Inc(I);
      end
      else
        InQuotes := True
    else if IsSpace(Ch) and not InQuotes then
    begin
      Attribute := SmartyTrim(Attribute);
      if Attribute <> '' then Result.Add(Attribute);
      Attribute := '';
    end
    else
      Attribute := Attribute + Ch;
  end;

  Attribute := SmartyTrim(Attribute);
  if Attribute <> '' then Result.Add(Attribute);

  // First-char "=" signs should append to previous
  for I := Result.Count - 1 downto 1 do
    if Result[I][1] = '=' then
    begin
      Result[I - 1] := Result[I - 1] + Result[I];
      Result.Delete(I);
    end;

  // First-char quotes should append to previous
  for I := Result.Count - 1 downto 1 do
    if (Result[I][1] = '"') and (Pos('=', Result[I - 1]) > 0) then
    begin
      Result[I - 1] := Result[I - 1] + Result[I];
      Result.Delete(I);
    end;
end;

class procedure TTemplateAction.CheckFunction(ACommand: TStringList;
      const AValid: array of string);
var
  I, J: Integer;
  ACounts: array of byte;
  Name, Value: string;
  Found: Boolean;
begin
  SetLength(ACounts, High(AValid) + 1);

  for I := 0 to ACommand.Count - 1 do
  begin
    ExtractFunctionItem(ACommand, I, Name, Value);
    Found := False;
    for J := 0 to High(AValid) do
      if CompareText(AValid[J], Name) = 0 then
      begin
        if ACounts[J] > 0 then
          raise ESmartyException.CreateResFmt(@sDuplicateAttribute, [Name])
        else
          ACounts[J] := 1;
        Found := True;
        Break;
      end;

    if not Found then
      raise ESmartyException.CreateResFmt(@sInvalidAttribute, [Name]);
  end;
end;

class function TTemplateAction.GetAttributeValue(ACommand: TStringList;
  const AAtribute: string; const ADefault: string): string;
var
  I: Integer;
  Name, Value: string;
begin
  for I := 0 to ACommand.Count - 1 do
  begin
    ExtractFunctionItem(ACommand, I, Name, Value);
    if CompareText(Name, AAtribute) = 0 then
    begin
      if Value = '' then
        Exit(ADefault)
      else
        Exit(Value);
    end;
  end;

  Result := ADefault;
end;

class procedure TTemplateAction.ExtractFunctionItem(ACommand: TStringList;
  Index: Integer; var Name, Value: string);
var
  S: string;
  I: Integer;
begin
  S := ACommand[Index];
  I := Pos('=', S);
  if I > 0 then
  begin
    Name := Copy(S, 1, I - 1);
    Value := AnsiDequotedStr(Copy(S, I + 1, Length(S) - I), '"');
  end
  else begin
    Name := S;
    Value := '';
  end;
end;

class procedure TTemplateAction.ParseVariable(const AVariable: string; AVarList: TVarList);
var
  I, J: Integer;
  Variable, S: string;
  Ch: Char;
  InArrayIndex, SkipNextDot: Boolean;
  Part: TVariablePart;
begin
  Variable := AnsiUpperCase(AVariable);

  I := 1;
  S := '';
  InArrayIndex := False;
  SkipNextDot := False;

  while I <= Length(Variable) do
  begin
    Ch := Variable[I];
    Inc(I);

    if SkipNextDot then
    begin
      SkipNextDot := False;
      if (Ch = '.') then Continue;
    end;

    if Ch = '[' then
    begin
      if InArrayIndex then
        raise ESmartyException.CreateResFmt(@sInvalidArrayIndex, [Variable]);
      InArrayIndex := True;
      if S <> '' then
      begin
        Part := S;
        S := '';
        AVarList.Add(Part);
      end;
    end
    else if Ch = ']' then
    begin
      if not InArrayIndex then
        raise ESmartyException.CreateResFmt(@sUnpairBrackets, [Variable]);
      InArrayIndex := False;
      SkipNextDot := True;
      if TryStrToInt(S, J) then
      begin
        S := '';
        Part := J;
        AVarList.Add(Part);
      end
      else
        raise ESmartyException.CreateResFmt(@sInvalidCharsInArrayIndex, [Variable]);
    end
    else if Ch = '.' then
    begin
      Part := S;
      S := '';
      AVarList.Add(Part);
    end
    else if CharInSet(Ch, ['A'..'Z', 'a'..'z', '_', '0'..'9']) then
      S := S + Ch
    else
      raise ESmartyException.CreateResFmt(@sInvalidVarChars, [Ch, Variable]);
  end;

  if InArrayIndex then
    raise ESmartyException.CreateResFmt(@sUnclosedBrackets, [Variable]);
  if S <> '' then
  begin
    Part := S;
    AVarList.Add(Part);
  end;
end;

class procedure TTemplateAction.GetVariableProperties(AEngine: TSmartyEngine;
  const AVariable: string; var Namespace: TNamespaceProvider; var Index: Integer;
  var VarName: string; var AVarList: TVarList);
var
  NamespaceIndex: Integer;
  NamespaceName: string;
begin
  ParseVariable(AVariable, AVarList);
  if (AVarList.Count > 0) and (AVarList[0].PartType = vptValue) then
  begin
    NamespaceName := AVarList[0];
    AVarList.DeleteElement(0);
    NamespaceIndex := AEngine.Namespaces.IndexOf(NamespaceName);
    if NamespaceIndex >= 0 then
    begin
      Namespace := TNamespaceProvider(AEngine.Namespaces.Objects[NamespaceIndex]);
      if AVarList.Count > 0 then
      begin
        if Namespace.IsIndexSupported then
        begin
          if (AVarList[0].PartType = vptIndex) then
          begin
            Index := AVarList[0];
            AVarList.DeleteElement(0);
          end
          else
            raise ESmartyException.CreateResFmt(@sNamespaceIndexMiss, [AVariable]);
        end
        else
          Index := -1;

        if (AVarList.Count > 0) and (AVarList[0].PartType = vptValue) then
        begin
          VarName := AVarList[0];
          AVarList.DeleteElement(0);
        end
        else
          raise ESmartyException.CreateResFmt(@sNamespaceVarMiss, [AVariable]);
      end
      else begin
      //detach variables
        Namespace := nil;
        Index := -1;
        VarName := NamespaceName;
      end;
    end
    else begin
      Namespace := nil;
      Index := -1;
      VarName := NamespaceName;
    end;
  end
  else
    raise ESmartyException.CreateResFmt(@sInvalidVariable, [AVariable]);
end;

class function TTemplateAction.IsAction(AEngine: TSmartyEngine;
  const ACommand: string; var AAction: TTemplateAction): Boolean;
begin
  Result := False;
  AAction := nil;
end;

{************* TRawOutputAction *************}

constructor TRawOutputAction.Create(AEngine: TSmartyEngine);
begin
  inherited Create(AEngine);
  FActionType := tatRawOutput;
end;

constructor TRawOutputAction.CreateOutput(AEngine: TSmartyEngine; const AOutput: string);
begin
  inherited Create(AEngine);
  FOutput := AOutput;
end;

function TRawOutputAction.Execute: string;
begin
  Result := FOutput;
end;

class function TRawOutputAction.IsAction(AEngine: TSmartyEngine;
  const ACommand: string; var AAction: TTemplateAction): Boolean;
begin
  AAction := nil;
  // { symbol
  if CompareText(ACommand, 'ldelim') = 0 then AAction := TRawOutputAction.CreateOutput(AEngine, '{')
  // } symbol
  else if CompareText(ACommand, 'rdelim') = 0 then AAction := TRawOutputAction.CreateOutput(AEngine, '}');

  Result := Assigned(AAction);
end;

{************* TModifierAction *************}

constructor TModifierAction.Create;
begin
  inherited Create;
  FModifier := nil;
  FParams := nil;
end;

destructor TModifierAction.Destroy;
begin
  if Assigned(FParams) then FParams.Free;
  inherited Destroy;
end;

{************* TVariableOutputAction *************}

constructor TVariableOutputAction.Create(AEngine: TSmartyEngine);
begin
  inherited Create(AEngine);
  FNamespace := nil;
  FVarDetails := TVarList.Create;
  FModifiers := TObjectList<TModifierAction>.Create(True);
  FActionType := tatVariableOutput;
end;

destructor TVariableOutputAction.Destroy;
begin
  FVarDetails.Finalize;
  FModifiers.Free;
  inherited Destroy;
end;

function TVariableOutputAction.Execute: string;
var
  I: Integer;
  VarRec: TVariableRecord;
  Temp: TVariableRecord;
  NeedFinalize: Boolean;
begin
  VarRec := FEngine.GetVariable(FNamespace, FIndex, FVarName, FVarDetails, NeedFinalize);
  try
    if FModifiers.Count > 0 then
    begin
      Temp := VarRec.Clone;
      try
        for I := 0 to FModifiers.Count - 1 do
          FModifiers[I].FModifier.ModifyVariable(Temp, FModifiers[I].FParams);
        if FEngine.AutoHTMLEncode then
          Result := HTMLEncode(Temp.ToString)
        else
          Result := Temp.ToString;
      finally
        Temp.Finalize;
      end;
    end
    else
      if FEngine.AutoHTMLEncode then
        Result := HTMLEncode(VarRec.ToString)
      else
        Result := VarRec.ToString
  finally
    if NeedFinalize then VarRec.Finalize;
  end;
end;

procedure TVariableOutputAction.SetVariable(AEngine: TSmartyEngine;
  const AVariable: string);
begin
  GetVariableProperties(AEngine, AVariable, FNamespace, FIndex, FVarName, FVarDetails);
end;

class function TVariableOutputAction.IsAction(AEngine: TSmartyEngine;
  const ACommand: string; var AAction: TTemplateAction): Boolean;
var
  I, J: Integer;
  VarAction: TVariableOutputAction;
  Ch: Char;
  Modifier, Variable, Param: string;
  Params: TStringList;
  InQuote: Boolean;
  MAction: TModifierAction;
begin
  if (ACommand[1] = '$') then
  begin
    Result := True;
    VarAction := TVariableOutputAction.Create(AEngine);
    AAction := VarAction;

    I := 2;
    Variable := '';

    Ch := GetChar(ACommand, I);
    while CharInSet(Ch, ['A'..'Z','a'..'z','_','.', '[', ']', '0'..'9']) do
    begin
      Variable := Variable + Ch;
      Inc(I);
      Ch := GetChar(ACommand, I);
    end;

    if Ch = #0 then
    begin
      VarAction.SetVariable(AEngine, Variable);
      Exit;
    end;

    while Ch <> #0 do
    begin
      if IsSpace(Ch) then
      begin
        Inc(I);
        while IsSpace(GetChar(ACommand, I)) do Inc(I);
        Ch := GetChar(ACommand, I);
      end;

      if Ch = #0 then
      begin
        VarAction.SetVariable(AEngine, Variable);
        Exit;
      end;

      if Ch = '|' then
      begin
        Inc(I);
        Modifier := '';
        Ch := GetChar(ACommand, I);

        if IsSpace(Ch) then
        begin
          Inc(I);
          while IsSpace(GetChar(ACommand, I)) do Inc(I);
          Ch := GetChar(ACommand, I);
        end;

        while not CharInSet(Ch, [#0..' ','{','}','|',':']) do
        begin
          Modifier := Modifier + Ch;
          Inc(I);
          Ch := GetChar(ACommand, I);
        end;

        if Ch = ':' then
        begin
          Params := TStringList.Create;
          InQuote := False;
          Param := '';
          Inc(I);

          while True do
          begin
            Ch := GetChar(ACommand, I);
            Inc(I);

            if (Ch = '"') then
            begin
              if InQuote then
                if GetChar(ACommand, I) = '"' then
                begin
                  Param := Param + '"';
                  Inc(I);
                end
                else
                  InQuote := False
              else
                InQuote := True;
            end
            else if (Ch = ':') and not InQuote then
            begin
              Params.Add(Param);
              Param := '';
            end
            else if (Ch = #0) or (not InQuote and (Ch = '|')) then
              Break
            else
              Param := Param + Ch;
          end;

          if Param <> '' then Params.Add(Param);
        end
        else
          Params := nil;

        MAction := TModifierAction.Create;
        MAction.FParams := Params;
        VarAction.FModifiers.Add(MAction);

        J := SmartyProvider.FModifiers.IndexOf(Modifier);
        if J >= 0 then
        begin
          MAction.FModifier := TVariableModifierClass(SmartyProvider.FModifiers.Objects[J]);
          if not MAction.FModifier.CheckInputParams(Params) then
          begin
            FreeAndNil(AAction);
            raise ESmartyException.CreateResFmt(@sInvalidModifierParams, [Modifier]);
          end;
        end
        else begin
          FreeAndNil(AAction);
          raise ESmartyException.CreateResFmt(@sInvalidModifier, [Modifier]);
        end;
      end
      else begin
        FreeAndNil(AAction);
        raise ESmartyException.CreateResFmt(@sInvalidTemplateChar, [Ch, ACommand]);
      end;
    end;

    VarAction.SetVariable(AEngine, Variable);
  end
  else begin
    Result := False;
    AAction := nil;
  end;
end;

{************* TFuncOutputAction *************}

constructor TFuncOutputAction.Create(AEngine: TSmartyEngine);
begin
  inherited Create(AEngine);
  FOperation := nil;
  FModifiers := TObjectList<TModifierAction>.Create(True);
  FActionType := tatFuncOutput;
end;

destructor TFuncOutputAction.Destroy;
begin
  FOperation.Free;
  FModifiers.Free;
  inherited Destroy;
end;

function TFuncOutputAction.Execute: string;
var
  I: Integer;
  VarRec: TVariableRecord;
  Temp: TVariableRecord;
  NeedFinalize: Boolean;
begin
  VarRec := FOperation.Evaluate(FEngine, NeedFinalize);
  try
    if FModifiers.Count > 0 then
    begin
      Temp := VarRec.Clone;
      try
        for I := 0 to FModifiers.Count - 1 do
          FModifiers[I].FModifier.ModifyVariable(Temp, FModifiers[I].FParams);
        if FEngine.AutoHTMLEncode then
          Result := HTMLEncode(Temp.ToString)
        else
          Result := Temp.ToString;
      finally
        Temp.Finalize;
      end;
    end
    else
      if FEngine.AutoHTMLEncode then
        Result := HTMLEncode(VarRec.ToString)
      else
        Result := VarRec.ToString;
  finally
    if NeedFinalize then VarRec.Finalize;
  end;
end;

class function TFuncOutputAction.IsAction(AEngine: TSmartyEngine;
  const ACommand: string; var AAction: TTemplateAction): Boolean;
var
  I, J: Integer;
  FuncAction: TFuncOutputAction;
  Ch: Char;
  FuncParams, Modifiers, Modifier, Param: string;
  FuncClass: TSmartyFunctionClass;
  Params: TStringList;
  InQuote: Boolean;
  MAction: TModifierAction;
begin
  if AEngine.IsFunction(ACommand, FuncClass, FuncParams, Modifiers) then
  begin
    Result := True;
    FuncAction := TFuncOutputAction.Create(AEngine);
    FuncAction.FOperation := TOperation.Parse(AEngine, FuncClass.GetName + '(' + FuncParams + ')');
    AAction := FuncAction;

    I := 1;

    Ch := GetChar(Modifiers, I);
    while Ch <> #0 do
    begin
      if IsSpace(Ch) then
      begin
        Inc(I);
        while IsSpace(GetChar(Modifiers, I)) do Inc(I);
        Ch := GetChar(Modifiers, I);
      end;

      if Ch = '|' then
      begin
        Inc(I);
        Modifier := '';
        Ch := GetChar(Modifiers, I);

        if IsSpace(Ch) then
        begin
          Inc(I);
          while IsSpace(GetChar(Modifiers, I)) do Inc(I);
          Ch := GetChar(Modifiers, I);
        end;

        while not CharInSet(Ch, [#0..' ','{','}','|',':']) do
        begin
          Modifier := Modifier + Ch;
          Inc(I);
          Ch := GetChar(Modifiers, I);
        end;

        if Ch = ':' then
        begin
          Params := TStringList.Create;
          InQuote := False;
          Param := '';
          Inc(I);

          while True do
          begin
            Ch := GetChar(Modifiers, I);
            Inc(I);

            if (Ch = '"') then
            begin
              if InQuote then
                if GetChar(Modifiers, I) = '"' then
                begin
                  Param := Param + '"';
                  Inc(I);
                end
                else
                  InQuote := False
              else
                InQuote := True;
            end
            else if (Ch = ':') and not InQuote then
            begin
              Params.Add(Param);
              Param := '';
            end
            else if (Ch = #0) or (not InQuote and (Ch = '|')) then
              Break
            else
              Param := Param + Ch;
          end;

          if Param <> '' then Params.Add(Param);
        end
        else
          Params := nil;

        MAction := TModifierAction.Create;
        MAction.FParams := Params;
        FuncAction.FModifiers.Add(MAction);

        J := SmartyProvider.FModifiers.IndexOf(Modifier);
        if J >= 0 then
        begin
          MAction.FModifier := TVariableModifierClass(SmartyProvider.FModifiers.Objects[J]);
          if not MAction.FModifier.CheckInputParams(Params) then
          begin
            FreeAndNil(AAction);
            raise ESmartyException.CreateResFmt(@sInvalidModifierParams, [Modifier]);
          end;
        end
        else begin
          FreeAndNil(AAction);
          raise ESmartyException.CreateResFmt(@sInvalidModifier, [Modifier]);
        end;
      end
      else begin
        FreeAndNil(AAction);
        raise ESmartyException.CreateResFmt(@sInvalidTemplateChar, [Ch, Modifiers]);
      end;
    end;
  end
  else begin
    Result := False;
    AAction := nil;
  end;
end;


{************* TOperation *************}

type
  TExpressionItem = class (TObject)
    constructor Create; virtual;
    destructor Destroy; override;
    class function ParseItem(AEngine: TSmartyEngine; const S: string;
      var Index: Integer; var Item: TExpressionItem): Boolean; virtual;   //only skip spaces and return False
    function CreateLink(AEngine: TSmartyEngine): TOperation; virtual;
    function GetLink: TOperation; virtual;
    procedure SetNilLink; virtual;
  {$IFDEF SMARTYDEBUG}
    function AsString: string; virtual;
  {$ENDIF}
  end;

  TVariableItem = class (TExpressionItem)  //$person.year
    VarName: string;
    Link: TOpVariable;
    constructor Create; override;
    destructor Destroy; override;
    class function IsItem(const S: string; const Index: Integer): Boolean;
    procedure ScanStr(const S: string; var Index: Integer);
    class function ParseItem(AEngine: TSmartyEngine; const S: string;
      var Index: Integer;  var Item: TExpressionItem): Boolean; override;
    function CreateLink(AEngine: TSmartyEngine): TOperation; override;
    function GetLink: TOperation; override;
    procedure SetNilLink; override;
  {$IFDEF SMARTYDEBUG}
    function AsString: string; override;
  {$ENDIF}
  end;

  TIdentifierItem = class (TExpressionItem) //True, False, null, shl, shr, is_null (function name)
    Name: string;
    Link: TOpFunction;
    constructor Create; override;
    destructor Destroy; override;
    class function IsItem(const S: string; const Index: Integer): Boolean;
    procedure ScanStr(const S: string; var Index: Integer);
    class function ParseItem(AEngine: TSmartyEngine; const S: string;
      var Index: Integer;  var Item: TExpressionItem): Boolean; override;
    function IsConstItem(var Item: TExpressionItem): Boolean;
    function IsOperatorItem(var Item: TExpressionItem): Boolean;
    function CreateLink(AEngine: TSmartyEngine): TOperation; override;
    function GetLink: TOperation; override;
    procedure SetNilLink; override;
  {$IFDEF SMARTYDEBUG}
    function AsString: string; override;
  {$ENDIF}
  end;

  TConstItem = class (TExpressionItem)
    NeedFinalize: Boolean;
    Value: TVariableRecord;
    Link: TOpConst;
    constructor Create; override;
    destructor Destroy; override;
    class function IsNumberItem(const S: string; const Index: Integer): Boolean;
    class function IsStringItem(const S: string; const Index: Integer): Boolean;
    class function IsDateTimeItem(const S: string; const Index: Integer): Boolean;
    class function IsDateLooseItem(const S: string; const Index: Integer): Boolean;
    procedure ScanNumberItem(const S: string; var Index: Integer);
    procedure ScanStringItem(const S: string; var Index: Integer; AParseEsapces: Boolean);
    procedure ScanDateItem(const S: string; Loose: Boolean; var Index: Integer);
    class function ParseItem(AEngine: TSmartyEngine; const S: string;
      var Index: Integer;  var Item: TExpressionItem): Boolean; override;
    function CreateLink(AEngine: TSmartyEngine): TOperation; override;
    function GetLink: TOperation; override;
    procedure SetNilLink; override;
  {$IFDEF SMARTYDEBUG}
    function AsString: string; override;
  {$ENDIF}
  end;

  TOperatorItem = class (TExpressionItem)
    Op: TOperator;
    Link: TOpOperator;
    constructor Create; override;
    destructor Destroy; override;
    function GetPrecedence: byte;
    class function IsItem(const S: string; const Index: Integer): Boolean;
    procedure ScanStr(const S: string; var Index: Integer);
    class function ParseItem(AEngine: TSmartyEngine; const S: string;
      var Index: Integer;  var Item: TExpressionItem): Boolean; override;
    function GetLink: TOperation; override;
    procedure SetNilLink; override;
  {$IFDEF SMARTYDEBUG}
    function AsString: string; override;
  {$ENDIF}
  end;

  TParenthesisType = (ptOpen {(}, ptClose {)}, ptComma {,});

  TParenthesisItem = class (TExpressionItem)
    ParenthesisType: TParenthesisType;
    constructor Create; override;
    destructor Destroy; override;
    class function IsItem(const S: string; const Index: Integer): Boolean;
    procedure ScanStr(const S: string; var Index: Integer);
    class function ParseItem(AEngine: TSmartyEngine; const S: string;
      var Index: Integer;  var Item: TExpressionItem): Boolean; override;
    function GetLink: TOperation; override;
    procedure SetNilLink; override;
  {$IFDEF SMARTYDEBUG}
    function AsString: string; override;
  {$ENDIF}
  end;

  TOpItem = class (TExpressionItem)
    Link: TOperation;
    constructor Create; override;
    destructor Destroy; override;
    class function ParseItem(AEngine: TSmartyEngine; const S: string;
      var Index: Integer;  var Item: TExpressionItem): Boolean; override;
    function GetLink: TOperation; override;
    procedure SetNilLink; override;
  {$IFDEF SMARTYDEBUG}
    function AsString: string; override;
  {$ENDIF}
  end;

{************* TExpressionItem *************}

constructor TExpressionItem.Create;
begin
  inherited Create;
end;

destructor TExpressionItem.Destroy;
begin
  inherited Destroy;
end;

class function TExpressionItem.ParseItem(AEngine: TSmartyEngine; const S: string;
  var Index: Integer; var Item: TExpressionItem): Boolean;
var
  Ch: Char;
begin
  Ch := GetChar(S, Index);
  while IsSpace(Ch) and not (Ch = #0) do
  begin
    Inc(Index);
    Ch := GetChar(S, Index);
  end;
  Result := False;
end;

function TExpressionItem.CreateLink(AEngine: TSmartyEngine): TOperation;
begin
  Result := nil;
end;

function TExpressionItem.GetLink: TOperation;
begin
  Result := nil;
end;

procedure TExpressionItem.SetNilLink;
begin
end;

{$IFDEF SMARTYDEBUG}
function TExpressionItem.AsString: string;
begin
  Result := '';
end;
{$ENDIF}

{************* TVariableItem *************}

constructor TVariableItem.Create;
begin
  inherited Create;
  Link := nil;
end;

destructor TVariableItem.Destroy;
begin
  if Assigned(Link) then Link.Free;
  inherited Destroy;
end;

class function TVariableItem.IsItem(const S: string; const Index: Integer): Boolean;
begin
  Result := GetChar(S, Index) = '$';
end;

procedure TVariableItem.ScanStr(const S: string; var Index: Integer);
var
  Ch: Char;
begin
  Inc(Index);
  Ch := GetChar(S, Index);
  VarName := '';
  while CharInSet(Ch, ['A'..'Z','a'..'z','_','.', '[', ']', '0'..'9']) do
  begin
    VarName := VarName + Ch;
    Inc(Index);
    Ch := GetChar(S, Index);
  end;
end;

class function TVariableItem.ParseItem(AEngine: TSmartyEngine; const S: string;
  var Index: Integer; var Item: TExpressionItem): Boolean;
begin
  inherited;

  Result := IsItem(S, Index);
  if Result then
  begin
    Item := TVariableItem.Create;
    TVariableItem(Item).ScanStr(S, Index);
  end;
end;

function TVariableItem.CreateLink(AEngine: TSmartyEngine): TOperation;
begin
  Link := TOpVariable.Create;
  Result := Link;
  TTemplateAction.GetVariableProperties(AEngine, VarName, Link.FNamespace,
    Link.FIndex, Link.FVarName, Link.FVarDetails);
end;

function TVariableItem.GetLink: TOperation;
begin
  Result := Link;
end;

procedure TVariableItem.SetNilLink;
begin
  Link := nil;
end;

{$IFDEF SMARTYDEBUG}
function TVariableItem.AsString: string;
begin
  Result := ' VAR%' + VarName + '% ';
end;
{$ENDIF}

{************* TIdentifierItem *************}

constructor TIdentifierItem.Create;
begin
  inherited Create;
  Link := nil;
end;

destructor TIdentifierItem.Destroy;
begin
  if Assigned(Link) then Link.Free;
  inherited Destroy;
end;

class function TIdentifierItem.IsItem(const S: string; const Index: Integer): Boolean;
begin
  Result := CharInSet(GetChar(S, Index), ['A'..'Z','a'..'z','_']);
end;

procedure TIdentifierItem.ScanStr(const S: string; var Index: Integer);
var
  Ch: Char;
begin
  Name := GetChar(S, Index);
  Inc(Index);
  Ch := GetChar(S, Index);
  while CharInSet(Ch, ['A'..'Z','a'..'z','_', '0'..'9']) do
  begin
    Name := Name + Ch;
    Inc(Index);
    Ch := GetChar(S, Index);
  end;
end;

class function TIdentifierItem.ParseItem(AEngine: TSmartyEngine; const S: string;
  var Index: Integer; var Item: TExpressionItem): Boolean;
begin
  inherited;

  Result := IsItem(S, Index);
  if Result then
  begin
    Item := TIdentifierItem.Create;
    TIdentifierItem(Item).ScanStr(S, Index);
  end;
end;

function TIdentifierItem.IsConstItem(var Item: TExpressionItem): Boolean;

  procedure CreateConstItem(Value: TVariableRecord);
  begin
    Item := TConstItem.Create;
    TConstItem(Item).Value := Value;
  end;

begin
  Result := True;
  if CompareText('true', Name) = 0 then CreateConstItem(True)
  else if CompareText('false', Name) = 0 then CreateConstItem(False)
  else if CompareText('null', Name) = 0 then CreateConstItem(TVariableRecord.Null)
  else Result := False;
end;

function TIdentifierItem.IsOperatorItem(var Item: TExpressionItem): Boolean;

  procedure CreateOperatorItem(Value: TOperator);
  begin
    Item := TOperatorItem.Create;
    TOperatorItem(Item).Op := Value;
  end;

begin
  Result := True;
  if CompareText('eq', Name) = 0 then CreateOperatorItem(opEq)
  else if CompareText('ne', Name) = 0 then CreateOperatorItem(opNeq)
  else if CompareText('neq', Name) = 0 then CreateOperatorItem(opNeq)
  else if CompareText('gt', Name) = 0 then CreateOperatorItem(opGt)
  else if CompareText('lt', Name) = 0 then CreateOperatorItem(opLt)
  else if CompareText('gte', Name) = 0 then CreateOperatorItem(opGte)
  else if CompareText('ge', Name) = 0 then CreateOperatorItem(opGte)
  else if CompareText('lte', Name) = 0 then CreateOperatorItem(opLte)
  else if CompareText('le', Name) = 0 then CreateOperatorItem(opLte)
  else if CompareText('seq', Name) = 0 then CreateOperatorItem(opSEq)
  else if CompareText('mod', Name) = 0 then CreateOperatorItem(opMod)
  else if CompareText('div', Name) = 0 then CreateOperatorItem(opDiv)
  else if CompareText('shl', Name) = 0 then CreateOperatorItem(opShl)
  else if CompareText('shr', Name) = 0 then CreateOperatorItem(opShr)
  else if CompareText('not', Name) = 0 then CreateOperatorItem(opLogicalNot)
  else if CompareText('and', Name) = 0 then CreateOperatorItem(opLogicalAnd)
  else if CompareText('or', Name) = 0 then CreateOperatorItem(opLogicalOr)
  else if CompareText('bitand', Name) = 0 then CreateOperatorItem(opBitwiseAnd)
  else if CompareText('bitor', Name) = 0 then CreateOperatorItem(opBitwiseOr)
  else if CompareText('xor', Name) = 0 then CreateOperatorItem(opBitwiseXor)
  else Result := False;
end;

function TIdentifierItem.CreateLink(AEngine: TSmartyEngine): TOperation;
begin
  Link := TOpFunction.Create;
  Result := Link;
  Link.FFuncClass := AEngine.GetFunction(Name);
  if not Assigned(Link.FFuncClass) then
    raise ESmartyException.CreateResFmt(@sInvalidFunction, [Name]);
end;

function TIdentifierItem.GetLink: TOperation;
begin
  Result := Link;
end;

procedure TIdentifierItem.SetNilLink;
begin
  Link := nil;
end;

{$IFDEF SMARTYDEBUG}
function TIdentifierItem.AsString: string;
begin
  Result := ' IDENT%' + Name + '% ';
end;
{$ENDIF}


{************* TConstItem *************}

constructor TConstItem.Create;
begin
  inherited Create;
  Link := nil;
  NeedFinalize := True;
end;

destructor TConstItem.Destroy;
begin
  if Assigned(Link) then
    Link.Free
  else
    if NeedFinalize then Value.Finalize;
  inherited Destroy;
end;

class function TConstItem.IsNumberItem(const S: string; const Index: Integer): Boolean;
var
  Ch: Char;
begin
  Ch := GetChar(S, Index);
  Result := CharInSet(Ch, ['0'..'9','.']) or
    (((Ch = '+') or (Ch = '-')) and CharInSet(GetChar(S, Index + 1), ['0'..'9','.']));
end;

class function TConstItem.IsStringItem(const S: string; const Index: Integer): Boolean;
begin
  Result := GetChar(S, Index) = '"';
end;

class function TConstItem.IsDateTimeItem(const S: string; const Index: Integer): Boolean;
begin
  Result := (GetChar(S, Index) = 'D') and (GetChar(S, Index + 1) = '"');
end;

class function TConstItem.IsDateLooseItem(const S: string; const Index: Integer): Boolean;
begin
  Result := (GetChar(S, Index) = 'L') and (GetChar(S, Index + 1) = '"');
end;

procedure TConstItem.ScanNumberItem(const S: string; var Index: Integer);
var
  Str: string;
  IntConst: Boolean;
  Ch: Char;
  I, J: Integer;
  D: Double;
begin
  IntConst := True;
  Str := GetChar(S, Index);
  Inc(Index);
  Ch := GetChar(S, Index);
  while CharInSet(Ch, ['0'..'9']) do
  begin
    Str := Str + Ch;
    Inc(Index);
    Ch := GetChar(S, Index);
  end;

  if Ch = '.' then
  begin
    IntConst := False;
    Str := Str + Ch;
    Inc(Index);
    Ch := GetChar(S, Index);

    while CharInSet(Ch, ['0'..'9']) do
    begin
      Str := Str + Ch;
      Inc(Index);
      Ch := GetChar(S, Index);
    end;
  end;

  if CharInSet(Ch, ['e', 'E']) then
  begin
    IntConst := False;
    Str := Str + Ch;
    Inc(Index);
    Ch := GetChar(S, Index);

    if CharInSet(Ch, ['-', '+']) then
    begin
      Str := Str + Ch;
      Inc(Index);
      Ch := GetChar(S, Index);
    end;

    while CharInSet(Ch, ['0'..'9']) do
    begin
      Str := Str + Ch;
      Inc(Index);
      Ch := GetChar(S, Index);
    end;
  end;

  if IntConst then
  begin
    Val(Str, I, J);
    if J = 0 then
      Value := I
    else
      raise ESmartyException.CreateResFmt(@sInvalidIntegerConst, [Str]);
  end
  else begin
    Val(Str, D, J);
    if J = 0 then
      Value := D
    else
      raise ESmartyException.CreateResFmt(@sInvalidFloatConst, [Str]);
  end;
end;

procedure TConstItem.ScanStringItem(const S: string; var Index: Integer;
  AParseEsapces: Boolean);
var
  Str: string;
  Ch: Char;
begin
  Inc(Index);
  Ch := GetChar(S, Index);
  Str := '';

  while Ch <> #0 do
  begin
    if Ch = '"' then
    begin
      if GetChar(S, Index + 1) = '"' then
      begin
        Str := Str + '"';
        Inc(Index);
      end
      else begin
        Inc(Index);
        Break;
      end;
    end
    else
      Str := Str + Ch;

    Inc(Index);
    Ch := GetChar(S, Index);
  end;

  if AParseEsapces then
    Value := ParseEscapes(Str)
  else
    Value := Str;
end;

procedure TConstItem.ScanDateItem(const S: string; Loose: Boolean; var Index: Integer);
var
  I: Integer;
  Str: string;
begin
  Inc(Index, 2);
  I := PosEx('"', S, Index);
  if I > 0 then
  begin
    Str := Copy(S, Index, I - Index + 1);
    Index := I + 1;

    try
      if Loose then
        Value := DateLooseFromString(Str)
      else
        Value := DateTimeFromString(Str);
    except
      raise ESmartyException.CreateResFmt(@sInvalidDateConst, [Str]);
    end;
  end;
end;

class function TConstItem.ParseItem(AEngine: TSmartyEngine; const S: string;
  var Index: Integer; var Item: TExpressionItem): Boolean;
begin
  inherited;

  Result := True;
  if IsNumberItem(S, Index) then
  begin
    Item := TConstItem.Create;
    TConstItem(Item).ScanNumberItem(S, Index);
  end
  else if IsStringItem(S, Index) then
  begin
    Item := TConstItem.Create;
    TConstItem(Item).ScanStringItem(S, Index, AEngine.AllowEspacesInStrings);
  end
  else if IsDateTimeItem(S, Index) then
  begin
    Item := TConstItem.Create;
    TConstItem(Item).ScanDateItem(S, False, Index);
  end
  else if IsDateLooseItem(S, Index) then
  begin
    Item := TConstItem.Create;
    TConstItem(Item).ScanDateItem(S, True, Index);
  end
  else
    Result := False;
end;

function TConstItem.CreateLink(AEngine: TSmartyEngine): TOperation;
begin
  Link := TOpConst.Create;
  Result := Link;
  Link.FValue := Value;
end;

function TConstItem.GetLink: TOperation;
begin
  Result := Link;
end;

procedure TConstItem.SetNilLink;
begin
  Link := nil;
  NeedFinalize := False;
end;

{$IFDEF SMARTYDEBUG}
function TConstItem.AsString: string;
begin
  Result := ' CONST%' + Value.ToString + '% ';
end;
{$ENDIF}

{************* TOperatorItem *************}

constructor TOperatorItem.Create;
begin
  inherited Create;
  Link := nil;
end;

destructor TOperatorItem.Destroy;
begin
  if Assigned(Link) then Link.Free;
  inherited Destroy;
end;

function TOperatorItem.GetPrecedence: byte;
begin
  Result := OperatorPrecedence[Op];
end;

class function TOperatorItem.IsItem(const S: string;
  const Index: Integer): Boolean;
var
  Ch: Char;
begin
  Ch := GetChar(S, Index);
  Result := CharInSet(Ch, ['!', '<', '>', '+', '-', '*', '/', '\', '%',
    '=', '&', '|', '~', '^']);
end;

procedure TOperatorItem.ScanStr(const S: string; var Index: Integer);
var
  Ch: Char;
begin
  Ch := GetChar(S, Index);
  Inc(Index);
  case Ch of
    '!' :
      begin
        case GetChar(S, Index) of
          '=':
            begin Op := opNeq; Inc(Index); end;
        else
          Op := opLogicalNot;
        end;
      end;

    '>' :
      begin
        case GetChar(S, Index) of
          '=':
            begin Op := opGte; Inc(Index); end;
          '>':
            begin Op := opShr; Inc(Index); end;
        else
          Op := opGt;
        end;
      end;
    '<' :
      begin
        case GetChar(S, Index) of
          '>':
            begin Op := opNeq; Inc(Index); end;
          '=':
            begin Op := opLte; Inc(Index); end;
          '<':
            begin Op := opShl; Inc(Index); end;
        else
          Op := opLt;
        end;
      end;

    '+' :
      Op := opAdd;
    '-' :
      Op := opSub;
    '*' :
      Op := opMply;
    '/' :
      Op := opDivide;
    '\' :
      Op := opDiv;
    '%' :
      Op := opMod;
    '=' :
      if GetChar(S, Index) = '=' then
      begin
        Op := opSEq; Inc(Index);
      end
      else
        Op := opEq;

    '&' :
      if GetChar(S, Index) = '&' then
      begin
        Op := opLogicalAnd; Inc(Index);
      end
      else
      Op := opBitwiseAnd;
    '|' :
      if GetChar(S, Index) = '|' then
      begin
        Op := opLogicalOr; Inc(Index);
      end
      else
        Op := opBitwiseOr;
    '~' :
      Op := opBitwiseNot;
    '^' :
      Op := opBitwiseXor;
  end;
end;

class function TOperatorItem.ParseItem(AEngine: TSmartyEngine; const S: string;
  var Index: Integer; var Item: TExpressionItem): Boolean;
begin
  inherited;

  Result := IsItem(S, Index);
  if Result then
  begin
    Item := TOperatorItem.Create;
    TOperatorItem(Item).ScanStr(S, Index);
  end;
end;

function TOperatorItem.GetLink: TOperation;
begin
  Result := Link;
end;

procedure TOperatorItem.SetNilLink;
begin
  Link := nil;
end;

{$IFDEF SMARTYDEBUG}
function TOperatorItem.AsString: string;
begin
  case Op of
    opEq:
      Result := ' = ';
    opNeq:
      Result := ' != ';
    opGt:
      Result := ' > ';
    opLt:
      Result := ' < ';
    opGte:
      Result := ' >= ';
    opLte:
      Result := ' <= ';
    opSEq:
      Result := ' == ';
    opAdd:
      Result := ' + ';
    opSub:
      Result := ' - ';
    opMply:
      Result := ' * ';
    opDivide:
      Result := ' / ';
    opMod:
      Result := ' % ';
    opDiv:
      Result := ' \ ';
    opShl:
      Result := ' << ';
    opShr:
      Result := ' >> ';
    opLogicalNot:
      Result := ' ! ';
    opLogicalAnd:
      Result := ' && ';
    opLogicalOr:
      Result := ' || ';
    opBitwiseNot:
      Result := ' ~ ';
    opBitwiseAnd:
      Result := ' & ';
    opBitwiseOr:
      Result := ' | ';
    opBitwiseXor:
      Result := ' ^ ';
  else
    Result := ' ILLEGAL ';
  end;
end;
{$ENDIF}

{************* TParenthesisItem *************}

constructor TParenthesisItem.Create;
begin
  inherited Create;
end;

destructor TParenthesisItem.Destroy;
begin
  inherited Destroy;
end;

class function TParenthesisItem.IsItem(const S: string; const Index: Integer): Boolean;
var
  Ch: Char;
begin
  Ch := GetChar(S, Index);
  Result := (Ch = '(') or (Ch = ')') or (Ch = ',');
end;

procedure TParenthesisItem.ScanStr(const S: string; var Index: Integer);
var
  Ch: Char;
begin
  Ch := GetChar(S, Index);
  if Ch = '(' then ParenthesisType := ptOpen
  else if Ch = ')' then ParenthesisType := ptClose
  else if Ch = ',' then ParenthesisType := ptComma;
  Inc(Index);
end;

class function TParenthesisItem.ParseItem(AEngine: TSmartyEngine; const S: string;
  var Index: Integer; var Item: TExpressionItem): Boolean;
begin
  inherited;

  Result := IsItem(S, Index);
  if Result then
  begin
    Item := TParenthesisItem.Create;
    TParenthesisItem(Item).ScanStr(S, Index);
  end;
end;

function TParenthesisItem.GetLink: TOperation;
begin
  Result := nil;
end;

procedure TParenthesisItem.SetNilLink;
begin
end;

{$IFDEF SMARTYDEBUG}
function TParenthesisItem.AsString: string;
begin
  case ParenthesisType of
    ptOpen:
      Result := ' ( ';
    ptComma:
      Result := ' , ';
    ptClose:
      Result := ' ) ';
  else
    Result := ' ILLEGAL ';
  end;
end;
{$ENDIF}

{************* TOpItem *************}

constructor TOpItem.Create;
begin
  inherited Create;
  Link := nil;
end;

destructor TOpItem.Destroy;
begin
  if Assigned(Link) then Link.Free;
  inherited Destroy;
end;

class function TOpItem.ParseItem(AEngine: TSmartyEngine; const S: string;
  var Index: Integer; var Item: TExpressionItem): Boolean;
begin
  Result := False;
end;

function TOpItem.GetLink: TOperation;
begin
  Result := Link;
end;

procedure TOpItem.SetNilLink;
begin
  Link := nil;
end;

{$IFDEF SMARTYDEBUG}
function TOpItem.AsString: string;
begin
  Result := ' %OPITEM% ';
end;
{$ENDIF}

{************* TOperation *************}

constructor TOperation.Create;
begin
  inherited Create;
end;

class function TOperation.Parse(AEngine: TSmartyEngine; const S: string): TOperation;

  {$IFDEF SMARTYDEBUG}
  procedure ShowExpr(AExpr: TObjectList<TExpressionItem>);
  var
    I: Integer;
    Str: string;
  begin
    Str := '';
    for I := 0 to AExpr.Count - 1 do
      Str := Str + AExpr[I].AsString;
    OutputDebugString(PChar(Str));
  end;
  {$ENDIF}

  procedure TransferTo(AFrom, ATo: TObjectList<TExpressionItem>; AIndex: Integer);
  begin
    ATo.Add(AFrom[AIndex]);
    AFrom.OwnsObjects := False;
    AFrom.Delete(AIndex);
    AFrom.OwnsObjects := True;
  end;

  function AnalyzeParseData(Data: TObjectList<TExpressionItem>): TOperation;
  var
    I, J, Stack: Integer;
    EItem, LItem, RItem: TExpressionItem;
    IdItem: TIdentifierItem;
    PnItem: TParenthesisItem;
    OpItem: TOperatorItem;
    OItem: TOpItem;
    FuncLink: TOpFunction;
    LOp, ROp: TOperation;
    Params: TObjectList<TExpressionItem>;
  begin
    {$IFDEF SMARTYDEBUG} ShowExpr(Data); {$ENDIF}

    I := 0;
    while I < Data.Count do
    begin
      EItem := Data[I];

      if (EItem is TVariableItem) or (EItem is TConstItem) then
      begin
        EItem.CreateLink(AEngine);
        Inc(I);
      end
      else if EItem is TIdentifierItem then
      begin
        IdItem := TIdentifierItem(EItem);
        FuncLink := TOpFunction(IdItem.CreateLink(AEngine));
        EItem := Data[I+1];

        if (EItem is TParenthesisItem) and (TParenthesisItem(EItem).ParenthesisType = ptOpen) then
        begin
          Data.Delete(I+1);
          Inc(I, 1);
          Stack := 1;
          Params := TObjectList<TExpressionItem>.Create;
          try
            while Stack > 0 do
            begin
              if I >= Data.Count then
                raise ESmartyException.CreateResFmt(@sUncloseFunctionDeclaration, [IdItem.Name]);

              EItem := Data[I];

              if EItem is TParenthesisItem then
              begin
                PnItem := TParenthesisItem(EItem);
                case PnItem.ParenthesisType of
                  ptOpen:
                    begin
                      Inc(Stack);
                      TransferTo(Data, Params, I);
                    end;

                  ptClose:
                    begin
                      Dec(Stack);
                      if Stack > 0 then
                         TransferTo(Data, Params, I)
                      else begin
                         Data.Delete(I);
                        FuncLink.FParams.Add(AnalyzeParseData(Params));
                        Params.Clear;
                      end;
                    end;

                  ptComma:
                    if Stack = 1 then
                    begin
                       Data.Delete(I);
                      FuncLink.FParams.Add(AnalyzeParseData(Params));
                      Params.Clear;
                    end
                    else
                      TransferTo(Data, Params, I);
                end;
              end
              else begin
                TransferTo(Data, Params, I);
              end;
            end;

          finally
            Params.Free;
          end;
        end
        else
          raise  ESmartyException.CreateResFmt(@sFunctionParamsMiss, [IdItem.Name]);
      end
      else if EItem is TParenthesisItem then
      begin
        if TParenthesisItem(EItem).ParenthesisType = ptOpen then
        begin
          Data.Delete(I);
          Stack := 1;
          Params := TObjectList<TExpressionItem>.Create;
          try
            while Stack > 0 do
            begin
              if I >= Data.Count then
                raise ESmartyException.CreateRes(@sPathensisDoNotClosed);

              EItem := Data[I];

              if EItem is TParenthesisItem then
              begin
                PnItem := TParenthesisItem(EItem);
                case PnItem.ParenthesisType of
                  ptOpen:
                    begin
                      Inc(Stack);
                      TransferTo(Data, Params, I);
                    end;

                  ptClose:
                    begin
                      Dec(Stack);
                      if Stack > 0 then
                        TransferTo(Data, Params, I)
                      else begin
                        Data.Delete(I);
                        Break;
                      end;
                    end;

                  ptComma:
                    if Stack = 1 then
                      raise ESmartyException.CreateRes(@sClosePathensisExpected)
                    else
                      TransferTo(Data, Params, I);
                end;
              end
              else begin
                TransferTo(Data, Params, I);
              end;
            end;

            OItem := TOpItem.Create;
            OItem.Link := AnalyzeParseData(Params);
            Data.Insert(I, OItem);

          finally
            Params.Free;
          end;

        end
        else begin
          {$IFDEF SMARTYDEBUG} ShowExpr(Data); {$ENDIF}
          raise ESmartyException.CreateRes(@sOpenPathensisExpected);
        end;

      end
      else
        Inc(I);
    end;


    for J := 1 to MaxPrecedence do
    begin
      I := 0;

      while I < Data.Count do
      begin
        EItem := Data[I];

        if (EItem is TOperatorItem) and (TOperatorItem(EItem).GetPrecedence = J) then
        begin
          OpItem := TOperatorItem(EItem);

          if OpItem.Op in [opLogicalNot, opBitwiseNot] then
          begin
            if I + 1 < Data.Count then
            begin
              RItem := Data[I+1];
              ROp := RItem.GetLink;
              if Assigned(ROp) then
              begin
                OpItem.Link := TOpOperator.Create;
                OpItem.Link.FOperator := OpItem.Op;
                OpItem.Link.FRightOp := ROp;
                RItem.SetNilLink;
                Data.Delete(I+1);
                Inc(I);
              end
              else
                raise ESmartyException.CreateRes(@sNotOperatorMissied);
            end
            else
              raise ESmartyException.CreateRes(@sExpressionExcepted);
          end
          else begin

            if (I + 1 < Data.Count) and (I > 0) then
            begin
              LItem := Data[I-1];
              RItem := Data[I+1];
              LOp := LItem.GetLink;
              ROp := RItem.GetLink;
              if Assigned(LOp) and Assigned(ROp) then
              begin
                OpItem.Link := TOpOperator.Create;
                OpItem.Link.FOperator := OpItem.Op;
                OpItem.Link.FLeftOp := LOp;
                OpItem.Link.FRightOp := ROp;
                LItem.SetNilLink;
                RItem.SetNilLink;
                Data.Delete(I+1);
                Data.Delete(I-1);
              end
              else
                raise ESmartyException.CreateRes(@sOperatorsMissed);
            end
            else
              raise ESmartyException.CreateRes(@sExpressionExcepted);

          end;
        end
        else
          Inc(I);
      end;
    end;

    if Data.Count = 1 then
    begin
      Result := Data[0].GetLink;
      Data[0].SetNilLink;
    end
    else begin
      {$IFDEF SMARTYDEBUG} ShowExpr(Data); {$ENDIF}
      raise ESmartyException.CreateRes(@sInvalidExpression);
    end;
  end;

var
  Index: Integer;
  Item, Item2: TExpressionItem;
  Expr: TObjectList<TExpressionItem>;

begin
  Result := nil;
  Expr := TObjectList<TExpressionItem>.Create;
  Index := 1;

  try
    while Index <= Length(S) do
    begin
      if TVariableItem.ParseItem(AEngine, S, Index, Item) or
         TConstItem.ParseItem(AEngine, S, Index, Item) or
         TIdentifierItem.ParseItem(AEngine, S, Index, Item) or
         TOperatorItem.ParseItem(AEngine, S, Index, Item) or
         TParenthesisItem.ParseItem(AEngine, S, Index, Item) then
      begin
         //morth identifier to const or operator item
        if (Item is TIdentifierItem) and
           (TIdentifierItem(Item).IsConstItem(Item2) or
            TIdentifierItem(Item).IsOperatorItem(Item2)) then
        begin
          FreeAndNil(Item);
          Item := Item2;
        end;

        Expr.Add(Item);
      end
      else if GetChar(S, Index) = #0 then Break
      else
        raise ESmartyException.CreateResFmt(@sInvalidCharInExpression, [GetChar(S, Index), S]);
    end;

    Result := AnalyzeParseData(Expr);
  finally
    Expr.Free;
  end;
end;

{************* TOpVariable *************}

constructor TOpVariable.Create;
begin
  inherited Create;
  FNamespace := nil;
  FIndex := -1;
  FVarName := '';
  FVarDetails := TVarList.Create;
end;

destructor TOpVariable.Destroy;
begin
  FVarDetails.Finalize;
  inherited Destroy;
end;

function TOpVariable.Evaluate(AEngine: TSmartyEngine; var NeedFinalize: Boolean): TVariableRecord;
begin
  Result := AEngine.GetVariable(FNamespace, FIndex, FVarName, FVarDetails, NeedFinalize);
end;

{$IFDEF SMARTYDEBUG}
function TOpVariable.AsString: string;
begin
  Result := ' VAR(' + FVarName + ') ';
end;
{$ENDIF}

{************* TOpConst *************}

constructor TOpConst.Create;
begin
  inherited Create;
end;

destructor TOpConst.Destroy;
begin
  FValue.Finalize;
  inherited Destroy;
end;

function TOpConst.Evaluate(AEngine: TSmartyEngine; var NeedFinalize: Boolean): TVariableRecord;
begin
  Result := FValue;
  NeedFinalize := False;
end;

{$IFDEF SMARTYDEBUG}
function TOpConst.AsString: string;
begin
  Result := ' CONST(' + FValue.ToString + ') ';
end;
{$ENDIF}

{************* TOpFunction *************}

constructor TOpFunction.Create;
begin
  inherited Create;
  FFuncClass := nil;
  FParams := TOperationList.Create;
end;

destructor TOpFunction.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

function TOpFunction.Evaluate(AEngine: TSmartyEngine; var NeedFinalize: Boolean): TVariableRecord;
var
  VarArray: array of TVariableRecord;
  FinArray: array of Boolean;
  I: Integer;
begin
  if Assigned(FFuncClass) then
  begin
    SetLength(VarArray, FParams.Count);
    SetLength(FinArray, FParams.Count);
    try
      for I := 0 to FParams.Count - 1 do
        VarArray[I] := FParams[I].Evaluate(AEngine, FinArray[I]);
      Result := FFuncClass.EvaluateFunction(VarArray);
      NeedFinalize := (Result.VarType = vtString) or (Result.VarType = vtArray);
    finally
      for I := 0 to FParams.Count - 1 do
        if FinArray[I] then VarArray[I].Finalize;
    end;
  end
  else begin
    Result := TVariableRecord.Null;
    NeedFinalize := False;
  end;
end;

{$IFDEF SMARTYDEBUG}
function TOpFunction.AsString: string;
var
  I: Integer;
begin
  Result := ' ' + FFuncClass.GetName + '(';
  for I := 0 to FParams.Count - 1 do
    Result := Result + FParams[I].AsString + ',';
  if FParams.Count > 0 then
    Delete(Result, Length(Result), 1);
  Result := Result + ') ';
end;
{$ENDIF}

{************* TOpOperator *************}

constructor TOpOperator.Create;
begin
  inherited Create;
  FLeftOp := nil;
  FRightOp := nil;
end;

destructor TOpOperator.Destroy;
begin
  if Assigned(FLeftOp) then FLeftOp.Free;
  if Assigned(FRightOp) then FRightOp.Free;
  inherited Destroy;
end;

function TOpOperator.Evaluate(AEngine: TSmartyEngine; var NeedFinalize: Boolean): TVariableRecord;
var
  LeftVar, RightVar: TVariableRecord;
  LeftFinalize, RightFinalize: Boolean;
begin
  LeftFinalize := False;
  RightFinalize := False;
  if not (FOperator in [opLogicalNot, opBitwiseNot]) then
    LeftVar := FLeftOp.Evaluate(AEngine, LeftFinalize);
  RightVar := FRightOp.Evaluate(AEngine, RightFinalize);

  case FOperator of
    opEq:
      Result := TVariableRecord.DoCompare(LeftVar, RightVar, coEq);
    opNeq:
      Result := TVariableRecord.DoCompare(LeftVar, RightVar, coNeq);
    opGt:
      Result := TVariableRecord.DoCompare(LeftVar, RightVar, coGt);
    opLt:
      Result := TVariableRecord.DoCompare(LeftVar, RightVar, coLt);
    opGte:
      Result := TVariableRecord.DoCompare(LeftVar, RightVar, coGte);
    opLte:
      Result := TVariableRecord.DoCompare(LeftVar, RightVar, coLte);
    opSEq:
      Result := TVariableRecord.DoCompare(LeftVar, RightVar, coSEq);
    opAdd:
      Result := TVariableRecord.DoIntFloatOp(LeftVar, RightVar, voAdd);
    opSub:
      Result := TVariableRecord.DoIntFloatOp(LeftVar, RightVar, voSubtract);
    opMply:
      Result := TVariableRecord.DoIntFloatOp(LeftVar, RightVar, voMultiply);
    opDivide:
      Result := TVariableRecord.DoFloatOp(LeftVar, RightVar, voDivide);
    opMod:
      Result := TVariableRecord.DoIntOp(LeftVar, RightVar, voModulus);
    opDiv:
      Result := TVariableRecord.DoIntOp(LeftVar, RightVar, voIntDivide);
    opShl:
      Result := TVariableRecord.DoIntOp(LeftVar, RightVar, voShl);
    opShr:
      Result := TVariableRecord.DoIntOp(LeftVar, RightVar, voShr);
    opLogicalNot:
      Result := TVariableRecord.DoLogicalNot(RightVar);
    opLogicalAnd:
      Result := TVariableRecord.DoLogicalOp(LeftVar, RightVar, voAnd);
    opLogicalOr:
      Result := TVariableRecord.DoLogicalOp(LeftVar, RightVar, voOr);
    opBitwiseNot:
      Result := TVariableRecord.DoIntNot(RightVar);
    opBitwiseAnd:
      Result := TVariableRecord.DoIntOp(LeftVar, RightVar, voAnd);
    opBitwiseOr:
      Result := TVariableRecord.DoIntOp(LeftVar, RightVar, voOr);
    opBitwiseXor:
      Result := TVariableRecord.DoIntOp(LeftVar, RightVar, voXor);
  else
    Result := TVariableRecord.Null;
  end;

  NeedFinalize := (Result.VarType = vtString) or (Result.VarType = vtArray);
  if RightFinalize then RightVar.Finalize;
  if LeftFinalize then LeftVar.Finalize;
end;

{$IFDEF SMARTYDEBUG}
function TOpOperator.AsString: string;
begin
  case FOperator of
    opEq:
      Result := ' %s = %s ';
    opNeq:
      Result := ' %s != %s ';
    opGt:
      Result := ' %s > %s ';
    opLt:
      Result := ' %s < %s ';
    opGte:
      Result := ' %s >= %s ';
    opLte:
      Result := ' %s <= %s ';
    opSEq:
      Result := ' %s == %s ';
    opAdd:
      Result := ' %s + %s ';
    opSub:
      Result := ' %s - %s ';
    opMply:
      Result := ' %s * %s ';
    opDivide:
      Result := ' %s / %s ';
    opMod:
      Result := ' %s % %s ';
    opDiv:
      Result := ' %s \ %s ';
    opShl:
      Result := ' %s << %s ';
    opShr:
      Result := ' %s >> %s ';
    opLogicalNot:
      Result := ' ! %s ';
    opLogicalAnd:
      Result := ' %s && %s ';
    opLogicalOr:
      Result := ' %s || %s ';
    opBitwiseNot:
      Result := ' ~ %s ';
    opBitwiseAnd:
      Result := ' %s & %s ';
    opBitwiseOr:
      Result := ' %s | %s ';
    opBitwiseXor:
      Result := ' %s ^ %s ';
  end;

  if FOperator in [opLogicalNot, opBitwiseNot] then
     Result := Format(Result, [FRightOp.AsString])
  else
    Result := Format(Result, [FLeftOp.AsString, FRightOp.AsString]);

  Result := ' (' + Result + ') ';
end;
{$ENDIF}

{************* TIfCondition *************}

constructor TIfCondition.Create(AEngine: TSmartyEngine);
begin
  inherited Create;
  FEngine := AEngine;
end;

{************* TSimpleIf *************}

constructor TSimpleIf.Create(AEngine: TSmartyEngine);
begin
  inherited Create(AEngine);
  FOperation := nil;
end;

constructor TSimpleIf.CreateOperation(AEngine: TSmartyEngine; AExpr: string);
begin
  Create(AEngine);
  FOperation := TOperation.Parse(AEngine, AExpr);
end;

destructor TSimpleIf.Destroy;
begin
  if Assigned(FOperation) then FOperation.Free;
  inherited Destroy;
end;

function TSimpleIf.Evaluate: Boolean;
var
  VarRec: TVariableRecord;
  NeedFinalize: Boolean;
begin
  if Assigned(FOperation) then
  begin
    VarRec := FOperation.Evaluate(FEngine, NeedFinalize);
    Result := VarRec.ToBool;
    if NeedFinalize then VarRec.Finalize;

  end
  else
    Result := False;
end;

{************* TVariableIf *************}

constructor TVariableIf.Create(AEngine: TSmartyEngine);
begin
  inherited Create(AEngine);
  FNamespace := nil;
  FVarDetails := TVarList.Create;
end;

constructor TVariableIf.CreateIf(AEngine: TSmartyEngine; AType: TIfType);
begin
  Create(AEngine);
  FIfType := AType;
end;

destructor TVariableIf.Destroy;
begin
  FVarDetails.Finalize;
  inherited Destroy;
end;

function TVariableIf.Evaluate: Boolean;
var
  VarRec: TVariableRecord;
  ArrayData: PVariableArray;
  NeedFinalize: Boolean;
begin
  VarRec := FEngine.GetVariable(FNamespace, FIndex, FVarName, FVarDetails, NeedFinalize);
  try
    if VarRec.IsArray then
    begin
      ArrayData := VarRec.AValue;
      case FIfType of
        ifDef:
          Result := True;
        ifNDef:
          Result := False;
        ifEmpty:
          Result := (ArrayData.Count > 0);
        ifNEmpty:
          Result := (ArrayData.Count = 0);
      else
        Result := False;
      end;
    end
    else begin
      case FIfType of
        ifDef:
          Result := VarRec.VarType <> vtNull;
        ifNDef:
          Result := VarRec.VarType = vtNull;
        ifEmpty:
          Result := VarRec.IsEmpty;
        ifNEmpty:
          Result := not VarRec.IsEmpty;
      else
        Result := False;
      end;
    end;
  finally
    if NeedFinalize then VarRec.Finalize;
  end;
end;

procedure TVariableIf.SetVariable(AEngine: TSmartyEngine; const AVariable: string);
var
  I: Integer;
  Variable: string;
begin
  Variable := SmartyTrim(AVariable);
  if (Length(Variable) > 2) and (Variable[1] = '$') then
  begin
    for I := 2 to Length(Variable) do
      if not CharInSet(Variable[I], ['A'..'Z','a'..'z','_','.', '[', ']', '0'..'9']) then
      begin
        raise ESmartyException.CreateResFmt(@sInvalidVarDeclaration, [Variable]);
      end;
  end
  else
    raise ESmartyException.CreateResFmt(@sInvalidVarDeclaration, [Variable]);

  Delete(Variable, 1, 1);                       //delete $ sign
  TTemplateAction.GetVariableProperties(AEngine, Variable, FNamespace, FIndex,
    FVarName, FVarDetails);
end;

{************* TElseIfAction *************}

constructor TElseIfAction.Create;
begin
  inherited Create;
  FCondition := nil;
  FActions := TTemplateActions.Create(True);
end;

destructor TElseIfAction.Destroy;
begin
  if Assigned(FCondition) then FCondition.Free;
  FActions.Free;
  inherited Destroy;
end;

{************* TIfOutputAction *************}

constructor TIfOutputAction.Create(AEngine: TSmartyEngine);
begin
  inherited Create(AEngine);
  FActionType := tatIf;

  FCondition := nil;
  FThenActions := TTemplateActions.Create;
  FElseActions := TTemplateActions.Create;
  FElseIfActions := TElseIfActions.Create(True);
end;

destructor TIfOutputAction.Destroy;
begin
  if Assigned(FCondition) then FCondition.Free;
  FElseIfActions.Free;
  FThenActions.Free;
  FElseActions.Free;
  inherited Destroy;
end;

function TIfOutputAction.ContinueIf(AEngine: TSmartyEngine; const ACommand: string;
  var AState: Integer; var AActions: TTemplateActions): Boolean;
var
  ElseAction: TElseIfAction;

  function AddVarIf(AIfType: TIfType; const S: string): Boolean;
  begin
    if AState >= 2 then
       raise ESmartyException.CreateRes(@sElseIfAfterElseBlock);
    AState := 1;

    ElseAction := TElseIfAction.Create;
    FElseIfActions.Add(ElseAction);
    ElseAction.FCondition := TVariableIf.CreateIf(AEngine, AIfType);
    TVariableIf(ElseAction.FCondition).SetVariable(AEngine, S);
    AActions := ElseAction.FActions;
    Result := True;
  end;

var
  Command: string;
begin
  Command := ACommand;
  if IsTag('/if', Command, True) then
  begin
    AState := 3;
    AActions := nil;
    Result := False;
  end
  else if IsTag('else', Command, True) then
  begin
    if AState >= 2 then
      raise ESmartyException.CreateRes(@sElseAfterElseBlock);
    AState := 2;
    AActions := FElseActions;
    Result := True;
  end
  else if IsTagAndGetCommand('elseif', Command) then
  begin
    if AState >= 2 then
       raise ESmartyException.CreateRes(@sElseIfAfterElseBlock);
    AState := 1;

    ElseAction := TElseIfAction.Create;
    FElseIfActions.Add(ElseAction);
    ElseAction.FCondition := TSimpleIf.CreateOperation(AEngine, Command);

    AActions := ElseAction.FActions;
    Result := True;
  end
  else if IsTagAndGetCommand('elseifdef', Command) then Result := AddVarIf(ifDef, Command)
  else if IsTagAndGetCommand('elseifndef', Command) then Result := AddVarIf(ifNDef, Command)
  else if IsTagAndGetCommand('elseifempty', Command) then Result := AddVarIf(ifEmpty, Command)
  else if IsTagAndGetCommand('elseifnempty', Command) then Result := AddVarIf(ifNEmpty, Command)
  else
    Result := False;
end;

function TIfOutputAction.Execute: string;
var
  I: Integer;
begin
  if FCondition.Evaluate then
    Result := FThenActions.Execute
  else begin
    for I := 0 to FElseIfActions.Count - 1 do
      if FElseIfActions[I].FCondition.Evaluate then
        Exit(FElseIfActions[I].FActions.Execute);
     Result := FElseActions.Execute;
  end;
end;

class function TIfOutputAction.IsAction(AEngine: TSmartyEngine;
  const ACommand: string; var AAction: TTemplateAction): Boolean;

  function AddVarIf(AIfType: TIfType; S: string): TIfOutputAction;
  begin
    Result := TIfOutputAction.Create(AEngine);
    Result.FCondition := TVariableIf.CreateIf(AEngine, AIfType);
    TVariableIf(Result.FCondition).SetVariable(AEngine, S);
  end;

var
  Command: string;
begin
  Command := ACommand;
  AAction := nil;
  if IsTagAndGetCommand('if', Command) then
  begin
    AAction := TIfOutputAction.Create(AEngine);
    TIfOutputAction(AAction).FCondition := TSimpleIf.CreateOperation(AEngine, Command);
  end
  else if IsTagAndGetCommand('ifdef', Command) then
    AAction := AddVarIf(ifDef, Command)
  else if IsTagAndGetCommand('ifndef', Command) then
    AAction := AddVarIf(ifNDef, Command)
  else if IsTagAndGetCommand('ifempty', Command) then
    AAction := AddVarIf(ifEmpty, Command)
  else if IsTagAndGetCommand('ifnempty', Command) then
    AAction := AddVarIf(ifNEmpty, Command);

  Result := Assigned(AAction);
end;

{************* TForEachAction *************}

constructor TForEachOutputAction.Create(AEngine: TSmartyEngine);
begin
  inherited Create(AEngine);
  FActionType := tatForEach;
  FNamespaceBased := False;

  FVarDetails := TVarList.Create;
  FBaseActions := TTemplateActions.Create;
  FElseActions := TTemplateActions.Create;
end;

destructor TForEachOutputAction.Destroy;
begin
  FVarDetails.Finalize;
  FBaseActions.Free;
  FElseActions.Free;
  inherited Destroy;
end;

function TForEachOutputAction.Execute: string;
var
  List: TForEachData;
  I, LMin, LMax: Integer;
  VarRec: TVariableRecord;
  NeedFinalize: Boolean;
begin
  if not FNamespaceBased then
    VarRec := FEngine.GetVariable(FNamespace, FIndex, FVarName, FVarDetails, NeedFinalize);
  Result := '';

  try
    List := TForEachData.Create;
    List.Name := FForEachName;
    List.InForEach := False;
    List.ItemVarName := FVariableName;
    List.KeyVarName := FAssocName;
    List.IsNamespace := FNamespaceBased;
    List.MinIndex := 0;

    if FNamespaceBased then
    begin
      List.Namespace := FNamespace;
      List.VarData := nil;
      FNamespace.GetIndexProperties(LMin, LMax);
      List.Show := (LMax - LMin) >= 0;
      List.MinIndex := LMin;
      if List.Show then
        List.Total := (LMax - LMin) + 1
      else
        List.Total := 0;
    end
    else begin
      List.Namespace := nil;
      List.VarData := VarRec.AValue;
      List.Show := VarRec.IsArray and (List.VarData.Count > 0);
      if List.Show then
        List.Total := List.VarData.Count
      else begin
        List.VarData := nil;
        List.Total := 0;
      end;
    end;

    if (FMaxItems > 0) and (FMaxItems < List.Total) then
      List.Total := FMaxItems;

    FEngine.SmartyNamespace.FForEachList.EnterForEach(List);
    try
      if List.Show then
      begin
        List.InForEach := True;

        for I := 1 to List.Total do
        begin
          List.Iteration := I;
          List.First := (I = 1);
          List.Last := (I = List.Total);
          Result := Result + FBaseActions.Execute;
        end
      end
      else
        Result := FElseActions.Execute;
    finally
       List.InForEach := False;
       FEngine.SmartyNamespace.FForEachList.ExitForEach;
    end;
  finally
    if not FNamespaceBased and NeedFinalize then VarRec.Finalize;
  end;
end;

function TForEachOutputAction.ContinueForEach(AEngine: TSmartyEngine;
  const ACommand: string; var AState: Integer; var AActions: TTemplateActions): Boolean;
var
  Command: string;
begin
  Command := ACommand;
  if IsTag('/foreach', Command, True) then
  begin
    AState := 2;
    AActions := nil;
    Result := False;
  end
  else if IsTag('foreachelse', Command, True) then
  begin
    if AState >= 2 then
      raise ESmartyException.CreateRes(@sForEachElseAfterBlockEnd);
    AState := 1;
    AActions := FElseActions;
    Result := True;
  end
  else
    Result := False;
end;

class function TForEachOutputAction.IsAction(AEngine: TSmartyEngine;
  const ACommand: string; var AAction: TTemplateAction): Boolean;
var
  SL: TStringList;
  S, VarName, Command: string;
  I, NamespaceIndex: Integer;
  NamespaceProvider: TNamespaceProvider;
begin
  Command := ACommand;
  AAction := nil;
  Result := False;
  if IsTagAndGetCommand('foreach', Command) then
  begin
    SL := ParseFunction(Command);
    try
      CheckFunction(SL, ['from', 'item', 'key', 'name', 'maxitems']);
      AAction := TForEachOutputAction.Create(AEngine);

      TForEachOutputAction(AAction).FNamespaceBased := False;
      TForEachOutputAction(AAction).FForEachName := GetAttributeValue(SL, 'name');
      TForEachOutputAction(AAction).FVariableName := GetAttributeValue(SL, 'item', 'item');
      TForEachOutputAction(AAction).FAssocName := GetAttributeValue(SL, 'key', 'key');
      S := GetAttributeValue(SL, 'maxitems', '0');
      if TryStrToInt(S, I) and (I > 0) then
        TForEachOutputAction(AAction).FMaxItems := I
      else
        TForEachOutputAction(AAction).FMaxItems := 0;

      VarName := GetAttributeValue(SL, 'from');
      if (Length(VarName) > 1) and (VarName[1] = '$') then
        Delete(VarName, 1, 1)
      else begin
        FreeAndNil(AAction);
        raise ESmartyException.CreateRes(@sFromVariableRequireForEach);
      end;

      GetVariableProperties(AEngine, VarName, TForEachOutputAction(AAction).FNamespace,
        TForEachOutputAction(AAction).FIndex, TForEachOutputAction(AAction).FVarName,
        TForEachOutputAction(AAction).FVarDetails);

      //find namespace foreach
      with TForEachOutputAction(AAction) do
        if FNamespace = nil then
        begin
          NamespaceIndex := AEngine.Namespaces.IndexOf(FVarName);
          if NamespaceIndex >= 0 then
          begin
            NamespaceProvider := TNamespaceProvider(AEngine.Namespaces.Objects[NamespaceIndex]);
            if NamespaceProvider.IsIndexSupported then
            begin
              //namespace foreach
              FNamespace := NamespaceProvider;
              FVarName := '';
              FNamespaceBased := True;
            end;
          end;
        end;

    finally
      SL.Free;
    end;

    Result := True;
  end;
end;


{************* TCaptureArrayAction *************}

constructor TCaptureArrayAction.Create(AEngine: TSmartyEngine);
begin
  inherited Create(AEngine);
  FActionType := tatCaptureArray;
  FVarDetails := TVarList.Create;
  FFilter := nil;
end;

destructor TCaptureArrayAction.Destroy;
begin
  FVarDetails.Finalize;
  if Assigned(FFilter) then FFilter.Free;
  inherited Destroy;
end;

function TCaptureArrayAction.Execute: string;
var
  VarRec, NewVar, ResVar: TVariableRecord;
  NeedFinalize, NeedResFinalize, Include: Boolean;
  ARec: PVariableArray;
  I: Integer;
  Indexes: TList<Integer>;
begin
  VarRec := FEngine.GetVariable(FNamespace, FIndex, FVarName, FVarDetails, NeedFinalize);
  try
    if VarRec.IsArray then
    begin
      ARec := VarRec.AValue;
      Indexes := TList<Integer>.Create;
      try
        FEngine.SmartyNamespace.FActiveCapture.Enter(FItemName, 0, ARec);
        try
          for I := 0 to ARec.Count - 1 do
          begin
            if Assigned(FFilter) then
            begin
              ResVar := FFilter.Evaluate(FEngine, NeedResFinalize);
              try
                Include := ResVar.ToBool;
              finally
                if NeedResFinalize then ResVar.Finalize;
              end;
            end
            else
              Include := True;

            if Include then Indexes.Add(I);
            FEngine.SmartyNamespace.FActiveCapture.IncIndex;
          end;
        finally
          FEngine.SmartyNamespace.FActiveCapture.Exit;
        end;

        if Indexes.Count > 0 then
        begin
          NewVar.SetArrayLength(Indexes.Count);
          for I := 0 to Indexes.Count - 1 do
          begin
            NewVar.SetArrayItemQ(I, ARec.Data[Indexes[I]].Key,
              ARec.Data[Indexes[I]].Item.Clone);
          end;
        end
        else
          NewVar := TVariableRecord.Null;

      finally
        Indexes.Free;
      end;
    end
    else begin
      NewVar := TVariableRecord.Null;
    end;

    FEngine.SmartyNamespace.SetCaptureItem(FVariableName, NewVar);
    Result := '';
  finally
    if NeedFinalize then VarRec.Finalize;
  end;
end;

class function TCaptureArrayAction.IsAction(AEngine: TSmartyEngine;
  const ACommand: string; var AAction: TTemplateAction): Boolean;
var
  SL: TStringList;
  VarName, S, Command: string;
begin
  Command := ACommand;
  AAction := nil;
  Result := False;
  if IsTagAndGetCommand('capturearray', Command) then
  begin
    SL := ParseFunction(Command);
    try
      CheckFunction(SL, ['from', 'item', 'variable', 'filter']);
      AAction := TCaptureArrayAction.Create(AEngine);

      TCaptureArrayAction(AAction).FItemName := GetAttributeValue(SL, 'item', 'item');
      TCaptureArrayAction(AAction).FVariableName := GetAttributeValue(SL, 'variable', 'var');
      VarName := GetAttributeValue(SL, 'from');
      if (Length(VarName) > 1) and (VarName[1] = '$') then
        Delete(VarName, 1, 1)
      else begin
        FreeAndNil(AAction);
        raise ESmartyException.CreateRes(@sFromVariableRequireForEach);
      end;

      GetVariableProperties(AEngine, VarName, TCaptureArrayAction(AAction).FNamespace,
        TCaptureArrayAction(AAction).FIndex, TCaptureArrayAction(AAction).FVarName,
        TCaptureArrayAction(AAction).FVarDetails);

      S := GetAttributeValue(SL, 'filter');
      if S <> '' then
        TCaptureArrayAction(AAction).FFilter := TOperation.Parse(AEngine, S);
    finally
      SL.Free;
    end;

    Result := True;
  end;
end;


{************* TReleaseArrayAction *************}

constructor TReleaseArrayAction.Create(AEngine: TSmartyEngine);
begin
  inherited Create(AEngine);
  FActionType := tatReleaseArray;
end;

destructor TReleaseArrayAction.Destroy;
begin
  inherited Destroy;
end;

function TReleaseArrayAction.Execute: string;
begin
  FEngine.SmartyNamespace.RemoveCaptureItem(FVariableName);
  Result := '';
end;

class function TReleaseArrayAction.IsAction(AEngine: TSmartyEngine;
  const ACommand: string; var AAction: TTemplateAction): Boolean;
var
  SL: TStringList;
  Command: string;
begin
  Command := ACommand;
  AAction := nil;
  Result := False;
  if IsTagAndGetCommand('releasearray', Command) then
  begin
    SL := ParseFunction(Command);
    try
      CheckFunction(SL, ['variable']);
      AAction := TReleaseArrayAction.Create(AEngine);
      TReleaseArrayAction(AAction).FVariableName := GetAttributeValue(SL, 'variable', 'var');
    finally
      SL.Free;
    end;

    Result := True;
  end;
end;

{************* TAssignAction *************}

constructor TAssignAction.Create(AEngine: TSmartyEngine);
begin
  inherited Create(AEngine);
  FActionType := tatAssign;
  FValue := nil;
end;

destructor TAssignAction.Destroy;
begin
  if Assigned(FValue) then FValue.Free;
  inherited Destroy;
end;

function TAssignAction.Execute: string;
var
  VarValue: TVariableRecord;
  NeedFinalize: Boolean;
begin
  if Assigned(FValue) then
  begin
    VarValue := FValue.Evaluate(FEngine, NeedFinalize);
    FEngine.SmartyNamespace.SetCaptureItem(FVariableName, VarValue);
  end;

  Result := '';
end;

class function TAssignAction.IsAction(AEngine: TSmartyEngine;
  const ACommand: string; var AAction: TTemplateAction): Boolean;
var
  SL: TStringList;
  S, Command: string;
begin
  Command := ACommand;
  AAction := nil;
  Result := False;

  //long assign form
  if IsTagAndGetCommand('assign', Command) then
  begin
    SL := ParseFunction(Command);
    try
      CheckFunction(SL, ['variable', 'value']);
      AAction := TAssignAction.Create(AEngine);

      TAssignAction(AAction).FVariableName := GetAttributeValue(SL, 'variable', 'var');
      S := GetAttributeValue(SL, 'value');
      if S <> '' then
        TAssignAction(AAction).FValue := TOperation.Parse(AEngine, S);
    finally
      SL.Free;
    end;

    Result := True;
  end
  //short assign form
  else begin
    //Not yet done, need to resolve conflict with variable output
  end;
end;

{************* TReleaseAction *************}

constructor TReleaseAction.Create(AEngine: TSmartyEngine);
begin
  inherited Create(AEngine);
  FActionType := tatRelease;
end;

destructor TReleaseAction.Destroy;
begin
  inherited Destroy;
end;

function TReleaseAction.Execute: string;
begin
  FEngine.SmartyNamespace.RemoveCaptureItem(FVariableName);
  Result := '';
end;

class function TReleaseAction.IsAction(AEngine: TSmartyEngine;
  const ACommand: string; var AAction: TTemplateAction): Boolean;
var
  SL: TStringList;
  Command: string;
begin
  Command := ACommand;
  AAction := nil;
  Result := False;
  if IsTagAndGetCommand('release', Command) then
  begin
    SL := ParseFunction(Command);
    try
      CheckFunction(SL, ['variable']);
      AAction := TReleaseAction.Create(AEngine);
      TReleaseAction(AAction).FVariableName := GetAttributeValue(SL, 'variable', 'var');
    finally
      SL.Free;
    end;

    Result := True;
  end;
end;


{************* TSmartyInfoProvider *************}

constructor TSmartyInfoProvider.Create;
begin
  inherited Create;

  FModifiers := TStringList.Create;
  FModifiers.CaseSensitive := False;
  FModifiers.Sorted := True;
  FFunctions := TStringList.Create;
  FFunctions.CaseSensitive := False;
  FFunctions.Sorted := True;

  Init;
end;

procedure TSmartyInfoProvider.Init;
begin
  //default modifiers
  AddModifier(TCapitalizeModifier);
  AddModifier(TCatModifier);
  AddModifier(TTrimModifier);
  AddModifier(TCountCharactersModifier);
  AddModifier(TCountParagraphsModifier);
  AddModifier(TCountWordsModifier);
  AddModifier(TDefaultModifier);
  AddModifier(THTMLEncodeModifier);
  AddModifier(THTMLEncodeAllModifier);
  AddModifier(TXMLEncodeModifier);
  AddModifier(TFileEncodeModifier);
  AddModifier(TDateFormatModifier);
  AddModifier(TFloatFormatModifier);
  AddModifier(TLowerModifier);
  AddModifier(TUpperModifier);
  AddModifier(TNl2BrModifier);
  AddModifier(TTruncateModifier);
  AddModifier(TStripModifier);
  AddModifier(TSpacifyModifier);
  AddModifier(TWordwrapModifier);
  AddModifier(TIndentModifier);
  AddModifier(TReplaceModifier);
  AddModifier(TStripTagsModifier);

  //default functions
  AddFunction(TIsNullFunction);
  AddFunction(TIsEmptyFunction);
  AddFunction(TIsBooleanFunction);
  AddFunction(TIsIntegerFunction);
  AddFunction(TIsFloatFunction);
  AddFunction(TIsNumberFunction);
  AddFunction(TIsDateStrictFunction);
  AddFunction(TIsDateLooseFunction);
  AddFunction(TIsDateTimeFunction);
  AddFunction(TIsDateFunction);
  AddFunction(TIsStringFunction);
  AddFunction(TIsArrayFunction);
  AddFunction(TArrayLengthFunction);
  AddFunction(TArrayIndexFunction);
  AddFunction(TArrayKeyFunction);
  AddFunction(TCountFunction);

  //String Functions
  AddFunction(TEchoFunction);
  AddFunction(TPrintFunction);
  AddFunction(THTMLEncodeFunction);
  AddFunction(THTMLEncodeAllFunction);
  AddFunction(TXMLEncodeFunction);
  AddFunction(TFileEncodeFunction);
  AddFunction(TTrimFunction);
  AddFunction(TTruncateFunction);
  AddFunction(TCapitalizeFunction);
  AddFunction(TCountCharactersFunction);
  AddFunction(TCountWordsFunction);
  AddFunction(TCountParagraphsFunction);
  AddFunction(TUpperCaseFunction);
  AddFunction(TLowerCaseFunction);
  AddFunction(TStripFunction);
  AddFunction(TStripTagsFunction);
  AddFunction(TSpacifyFunction);
  AddFunction(TWordwrapFunction);
  AddFunction(TIndentFunction);
  AddFunction(TFloatFormatFunction);
  AddFunction(TIfThenFunction);

  AddFunction(TResemblesFunction);
  AddFunction(TContainsFunction);
  AddFunction(TStartsFunction);
  AddFunction(TEndsFunction);
  AddFunction(TReplaceFunction);


  //DateTime Functions
  AddFunction(TDateFormatFunction);
  AddFunction(TFullYearsFunction);
  AddFunction(TYearOfFunction);
  AddFunction(TMonthOfFunction);
  AddFunction(TDayOfFunction);
end;

procedure TSmartyInfoProvider.AddModifier(AModifier: TVariableModifierClass);
var
  Name: string;
begin
  Name := AModifier.GetName;
  if FModifiers.IndexOf(Name) >= 0 then
    raise ESmartyException.CreateRes(@sDuplicateModifierName);
  FModifiers.AddObject(Name, TObject(AModifier));
end;

procedure TSmartyInfoProvider.AddFunction(AFunction: TSmartyFunctionClass);
var
  Name: string;
begin
  Name := AFunction.GetName;
  if FFunctions.IndexOf(Name) >= 0 then
    raise ESmartyException.CreateRes(@sDuplicateFunctionName);
  FFunctions.AddObject(Name, TObject(AFunction));
end;

procedure TSmartyInfoProvider.DeleteFunction(AFunction: TSmartyFunctionClass);
var
  I: Integer;
begin
  I := FFunctions.IndexOf(AFunction.GetName);
  if I >= 0 then FFunctions.Delete(I);
end;

destructor TSmartyInfoProvider.Destroy;
begin
  FModifiers.Free;
  FFunctions.Free;
  inherited Destroy;
end;


{************* TSmartyEngine *************}

constructor TSmartyEngine.Create;
begin
  inherited Create;

  FCompiled := False;
  FActions := TTemplateActions.Create;
  FIsCache := True;
  FVarCache := TList<TVariableCache>.Create;

  FNamespaces := TStringList.Create;
  FNamespaces.CaseSensitive := False;
  FNamespaces.Sorted := True;

  FSilentMode := True;
  FErrors := TStringList.Create;
  FAutoHTMLEncode := False;
  FAllowEspacesInStrings := True;
  Init;
end;

procedure TSmartyEngine.Init;
begin
  //default namespaces
  FSmartyNamespace := TSmartyProvider.Create(Self);
  AddNamespace(FSmartyNamespace);
end;

procedure TSmartyEngine.SetIsCache(Value: Boolean);
begin
  if Value <> FIsCache then
  begin
    FIsCache := Value;
    if not FIsCache then ClearCache;
  end;
end;

procedure TSmartyEngine.ClearCache;
var
  I: Integer;
begin
  if FVarCache.Count > 0 then
    for I := 0 to FVarCache.Count - 1 do
    begin
      FVarCache[I].VariableValue.Finalize;
      FreeMem(FVarCache[I].VariableValue, SizeOf(TVariableRecord));
    end;
  FVarCache.Clear;
end;

procedure TSmartyEngine.ClearCache(ANamespace: TNamespaceProvider);
var
  I: Integer;
begin
  if FVarCache.Count > 0 then
    for I := FVarCache.Count - 1 downto 0 do
      if FVarCache[I].Namespace = ANamespace then
      begin
        FVarCache[I].VariableValue.Finalize;
        FreeMem(FVarCache[I].VariableValue, SizeOf(TVariableRecord));
        FVarCache.Delete(I);
      end;
end;

function TSmartyEngine.GetVariable(const ANamespace: TNamespaceProvider;
  AIndex: Integer; const AVariableName: string; ADetails: TVarList;
  var NeedFinalize: Boolean): TVariableRecord;
var
  I: Integer;
  CacheItem: TVariableCache;
  VarFound: Boolean;
  V: TVariableRecord;
begin
  if ANamespace = FSmartyNamespace then
  begin
    Result := FSmartyNamespace.GetSmartyVariable(AVariableName, ADetails,
      NeedFinalize);
  end
  else if ANamespace = nil then
  begin
    Result := FSmartyNamespace.GetDetachVariable(AVariableName, ADetails,
      NeedFinalize);
  end
  else begin
    //Find in cache
    VarFound := False;
    NeedFinalize := False;

    if FIsCache then
      for I := 0 to FVarCache.Count - 1 do
      begin
        CacheItem := FVarCache[I];
        if (CacheItem.Namespace = ANamespace) and
           ((AIndex = -1) or (AIndex = CacheItem.Index)) and
           (CacheItem.VariableName = AVariableName) then
        begin
          VarFound := True;
          Break;
        end;
      end;

    //If Found
    if VarFound then
    begin
      if ADetails.Count > 0 then
        Result := GetVariableDetails(CacheItem.VariableValue^, ADetails)
      else
        Result := CacheItem.VariableValue^;
    end
    else if Assigned(ANamespace) then  //If not found
    begin
      if FIsCache and ANamespace.UseCache then
      begin
        CacheItem.Namespace := ANamespace;
        CacheItem.Index := AIndex;
        CacheItem.VariableName := AVariableName;
        CacheItem.VariableValue := AllocMem(SizeOf(TVariableRecord));
        CacheItem.VariableValue^ := ANamespace.GetVariable(AIndex, AVariableName);
        FVarCache.Add(CacheItem);

        if ADetails.Count > 0 then
          Result := GetVariableDetails(CacheItem.VariableValue^, ADetails)
        else
          Result := CacheItem.VariableValue^;
      end
      else begin
        V := ANamespace.GetVariable(AIndex, AVariableName);
        if ADetails.Count > 0 then
        begin
          Result := GetVariableDetails(V, ADetails).Clone;
          V.Finalize;
        end
        else
          Result := V;

        NeedFinalize := ((Result.VarType = vtString) or (Result.VarType = vtArray));
      end;
    end
    else
      Result := TVariableRecord.Null;
  end;
end;

function TSmartyEngine.GetVariableDetails(const AVariable: TVariableRecord;
  ADetails: TVarList): TVariableRecord;
var
  Value: string;
  I, J, Index: Integer;
  ValueSet: Boolean;
  ArrayData: PVariableArray;
begin
  if ADetails.Count > 0 then
  begin
    Result := AVariable;

    for I := 0 to ADetails.Count - 1 do
    begin
      if not Result.IsArray then Exit(TVariableRecord.Null);

      case ADetails[I].PartType of
        vptValue:
        begin
          Value := ADetails[I];
          ArrayData := Result.AValue;
          ValueSet := False;
           for J := 0 to ArrayData.Count - 1 do
            if (ArrayData.Data[J].Key <> '') and
              (CompareText(ArrayData.Data[J].Key, Value) = 0) then
            begin
              Result := ArrayData.Data[J].Item;
              ValueSet := True;
               Break;
            end;

          if not ValueSet then Exit(TVariableRecord.Null);
        end;

        vptIndex:
        begin
          Index := ADetails[I];
          ArrayData := Result.AValue;
          if (Index >= 0) and (Index < ArrayData.Count) then
            Result := ArrayData.Data[Index].Item
          else
            Exit(TVariableRecord.Null);
        end;
      else
        Exit(TVariableRecord.Null);
      end;
    end;
  end
  else
    Result := AVariable;
end;

class function TSmartyEngine.IsFunction(const ACommand: string; var Func: TSmartyFunctionClass;
  var Params: string; var Modifiers: string): Boolean;
var
  I, J, K: Integer;
  Ch: Char;
  InQuote: Boolean;
  Stack: Integer;
  Command, SFunc: string;
begin
  Command := SmartyTrim(ACommand);
  Result := False;
  K := Pos('(', Command);
  if K > 0 then
  begin
    SFunc := SmartyTrim(Copy(Command, 1, K - 1));
    I := SmartyProvider.FFunctions.IndexOf(SFunc);

    if I >= 0 then
    begin
      Func := TSmartyFunctionClass(SmartyProvider.FFunctions.Objects[I]);

      InQuote := False;
      Stack := 0;

      for J := K to Length(Command) do
      begin
        Ch := Command[J];

        if not InQuote then
        begin
          if Ch = '(' then Inc(Stack)
          else if Ch = ')' then
          begin
            Dec(Stack);
            if Stack <= 0 then
            begin
              Params := SmartyTrim(Copy(Command, Length(SmartyProvider.FFunctions[I]) + 2,
                J - Length(SmartyProvider.FFunctions[I]) - 2));

              if Length(Command) > J then
                Modifiers := SmartyTrim(Copy(Command, J + 1, Length(Command) - J))
              else
                Modifiers := '';

              Break;
            end;
          end
          else if Ch = '"' then
            InQuote := True;
        end
        else if Ch = '"' then
          InQuote := False;
      end;

      if Stack > 0 then
        raise ESmartyException.CreateResFmt(@sInvalidFunctionDeclaration, [Command]);

      Exit(True);
    end;
  end;
end;

class function TSmartyEngine.GetFunction(const AFunction: string): TSmartyFunctionClass;
var
  I: Integer;
begin
  I := SmartyProvider.FFunctions.IndexOf(AFunction);

  if I >= 0 then
    Result := TSmartyFunctionClass(SmartyProvider.FFunctions.Objects[I])
  else
    Result := nil;
end;

destructor TSmartyEngine.Destroy;
var
  I: Integer;
begin
  FActions.Free;

  ClearCache;
  FVarCache.Free;

  FErrors.Free;

  for I := 0 to FNamespaces.Count - 1 do
    FNamespaces.Objects[I].Free;
  FNamespaces.Free;
  inherited Destroy;
end;

procedure TSmartyEngine.AddNamespace(ANamespace: TNamespaceProvider);
var
  Name: string;
begin
  Name := ANamespace.GetName;
  if FNamespaces.IndexOf(Name) >= 0 then
    raise ESmartyException.CreateRes(@sDuplicateNamespaceName);
  FNamespaces.AddObject(Name, ANamespace);
end;

procedure TSmartyEngine.DeleteNamespace(ANamespace: TNamespaceProvider);
var
  I: Integer;
begin
  I := FNamespaces.IndexOf(ANamespace.GetName);
  if I >= 0 then FNamespaces.Delete(I);
end;

function TSmartyEngine.Compile(const ADocument: string; var Errors: TStringList): Boolean;

  function SkipAllLiteralEnd(S: string; var IndexStart, IndexEnd: Integer): Boolean;
  var
    Pos1, Pos2: Integer;
    Str: string;
  begin
    while True do
    begin
      Pos1 := PosEx('{', S, IndexStart);
      if Pos1 > 0 then
      begin
        Pos2 := PosEx('}', S, Pos1);
        if Pos2 <= 0 then Exit(False);
        Str := Copy(S, Pos1 + 1, Pos2 - Pos1 - 1);
        Str := SmartyTrim(Str);
        if CompareText(Str, '/literal') = 0 then
        begin
          IndexStart := Pos1;
          IndexEnd := Pos2;
          Exit(True);
        end
        else
          Inc(IndexStart);
      end
      else
        Exit(False);
    end;
  end;

  // Skip exactly one new line (#10 or #13#10)
  procedure SkipLineBreak(S: string; var IndexStart: Integer);
  begin
    case GetChar(S, IndexStart) of
      Char(#10): 
        Inc(IndexStart);
      Char(#13): 
        if GetChar(S, IndexStart + 1) = Char(#10) then
          Inc(IndexStart, 2);
    end;
  end;

  function CompilePart(S: string; AActions: TTemplateActions;
    BreakAction: TNestAction; var AStart: Integer): string;
  var
    Output, Return, Tag: string;
    Ch: Char;
    J, K, State: Integer;
    InSmarty, InSmartyQuote: Boolean;
    Action: TTemplateAction;
    Acts: TTemplateActions;
  begin
    InSmarty := False;
    InSmartyQuote := False;
    Output := '';
    Tag := '';

    while AStart <= Length(S) do
    begin
      Ch := S[AStart];
      Inc(AStart);

      if InSmarty and (Ch = '"') then
      begin
        InSmartyQuote := not InSmartyQuote;
        Tag := Tag + Ch;
      end
      // Tag started
      else if not InSmartyQuote and (Ch = '{') then
      begin
        if not InSmarty then
        begin
          Tag := '';
          InSmarty := True;
        end
        else
          raise ESmartyException.CreateRes(@sOpenBraceInTemplate);
      end
      // Tag closed
      else if not InSmartyQuote and (Ch = '}') then
      begin
        if InSmarty then
        begin
          if CompareText(SmartyTrim(Tag), 'literal') = 0 then
          begin
            J := AStart;
            if SkipAllLiteralEnd(S, J, K) then
            begin
              Output := Output + Copy(S, AStart, J - AStart);
              AStart := K + 1;
            end
            else
              raise ESmartyException.CreateRes(@sLiteralDoNotFound);
          end
          else begin
            if Output <> '' then AActions.Add(TRawOutputAction.CreateOutput(Self, Output));
            Output := '';

            if not TTemplateAction.IsComment(Tag) then

              if TTemplateAction.IsExitCommand(Tag, BreakAction) then
              begin
                if FStripLineBreaksAfterBlocks then
                  SkipLineBreak(S, AStart);
                Exit(Tag);
              end

              else if TRawOutputAction.IsAction(Self, Tag, Action) or
                TVariableOutputAction.IsAction(Self, Tag, Action) or
                TFuncOutputAction.IsAction(Self, Tag, Action) or
                TCaptureArrayAction.IsAction(Self, Tag, Action) or
                TReleaseArrayAction.IsAction(Self, Tag, Action) or
                TAssignAction.IsAction(Self, Tag, Action) or
                TReleaseAction.IsAction(Self, Tag, Action) then
              begin
                AActions.Add(Action);
              end

              else if TIfOutputAction.IsAction(Self, Tag, Action) then
              begin
                AActions.Add(Action);
                State := 0;
                if FStripLineBreaksAfterBlocks then
                  SkipLineBreak(S, AStart);

                 Return := CompilePart(S, TIfOutputAction(Action).FThenActions, naIf, AStart);

                while TIfOutputAction(Action).ContinueIf(Self, Return, State, Acts) do
                  Return := CompilePart(S, Acts, naIf, AStart);
              end
              else if TForEachOutputAction.IsAction(Self, Tag, Action) then
              begin
                AActions.Add(Action);
                State := 0;
                if FStripLineBreaksAfterBlocks then
                  SkipLineBreak(S, AStart);

                 Return := CompilePart(S, TForEachOutputAction(Action).FBaseActions, naForEach, AStart);

                while TForEachOutputAction(Action).ContinueForEach(Self, Return, State, Acts) do
                  Return := CompilePart(S, Acts, naForEach, AStart);
              end
              else
                raise ESmartyException.CreateResFmt(@sInvalidTemplateDirective, [Tag]);

          end;

          if InSmartyQuote then
            raise ESmartyException.CreateRes(@sUncloseQuote);
          InSmarty := False;
        end
        else
          raise ESmartyException.CreateRes(@sCloseBraceWithoutTemplate);
      end
      // Tag lasts - collect it
      else if InSmarty then
        Tag := Tag + Ch
      // Raw output lasts - collect it
      else
        Output := Output + Ch;
    end;

    if InSmarty then
      raise ESmartyException.CreateRes(@sBraceDoNotClose);
    if Output <> '' then AActions.Add(TRawOutputAction.CreateOutput(Self, Output));
    Result := '';
  end;

  procedure GetLinePosition(APosition: Integer; var Line: Integer; var Pos: Integer);
  var
    J: Integer;
    Ch: Char;
  begin
    Line := 1;
    Pos := 1;
    J := 1;

    while (J <= Length(ADocument)) and (J <= APosition) do
    begin
      Ch := ADocument[J];
      Inc(J);
      if Ch = #13 then begin Inc(Line); Pos := 1; end
      else Inc(Pos);
    end;
  end;

var
  Str: string;
  I, L, P: Integer;

begin
  Result := False;
  FCompiled := False;
  FActions.Clear;
  Errors.Clear;
  I := 1;

  try
    Str := CompilePart(ADocument, FActions, naNone, I);
    if Str <> '' then
      ESmartyException.CreateResFmt(@sInvalidTemplateDirective, [Str])
    else
      FCompiled := True;
    Result := True;
  except
    on E: ESmartyException do
    begin
      GetLinePosition(I, L, P);
      Errors.Add(E.Message + Format(sAtPosition, [L, P]));
    end;
  end;
end;

function TSmartyEngine.Execute: string;
var
  I: Integer;
begin
  Result := '';
  if FCompiled then
    for I := 0 to FActions.Count - 1 do
      Result := Result + FActions[I].Execute;
end;

function TSmartyEngine.Execute(const Pattern: string; out Errors, Output: string): Boolean;
var
  ErrorList: TStringList;
begin
  ErrorList := TStringList.Create;
  try
    Result := Compile(Pattern, ErrorList);
    if Result then
      Output := Execute
    else
      Errors := ErrorList.Text;
  finally
    FreeAndNil(ErrorList);
  end;
end;

{************* Utilities *************}

function DateRecordToStr(var Value: TDateRecord): string;
var
  Date: TDateTime;
  IsYear, IsMonth, IsDay: Boolean;
  S: string;
begin
  IsYear := False;
  IsMonth := False;
  IsDay := False;
  if Value.Year = 0 then Value.Year := YearOf(Now) else IsYear := True;
  if Value.Month = 0 then Value.Month := 1 else IsMonth := True;
  if Value.Day = 0 then Value.Day := 1 else IsDay := True;
  if not IsYear and not IsMonth and not IsDay then Exit('');

  if TryEncodeDate(Value.Year, Value.Month, Value.Day, Date) then
  begin
    if IsYear and IsMonth and IsDay then
      Result := DateToStr(Date)
    else begin
      S := FormatSettings.ShortDateFormat;
      if not IsYear then S := StringReplace(S, 'y', '', [rfReplaceAll, rfIgnoreCase]);
      if not IsMonth then S := StringReplace(S, 'm', '', [rfReplaceAll, rfIgnoreCase]);
      if not IsDay then S := StringReplace(S, 'd', '', [rfReplaceAll, rfIgnoreCase]);
      while (Length(S) > 0) and not CharInSet(S[1], ['D', 'M', 'Y', 'y', 'm', 'd']) do
        Delete(S, 1, 1);
      while (Length(S) > 0) and not CharInSet(S[Length(S)], ['D', 'M', 'Y', 'y', 'm', 'd']) do
        Delete(S, Length(S), 1);
      if Length(S) > 0 then
        DateTimeToString(Result, S, Date)
      else
        Result := '';
    end;
  end
  else
    Result := '';
end;

function DateRecordToString(const Value: TDateRecord): string;
begin
  Result := Format('%.4d-%.2d-%.2d', [Value.Year, Value.Month, Value.Day]);
end;

function StringToDateRecord(const Value: string): TDateRecord;
begin
  Result.Year := StrToInt(Copy(Value, 1, 4));
  Result.Month := StrToInt(Copy(Value, 6, 2));
  Result.Day := StrToInt(Copy(Value, 9, 2));
end;

function DateTimeFromRecord(const Value: TDateRecord): TDateTime;
var
  Year, Month, Day: Word;
begin
  if Value.Year = 0 then Year := YearOf(Now) else Year := Value.Year;
  if Value.Month = 0 then Month := 1 else Month := Value.Month;
  if Value.Day = 0 then Day := 1 else Day := Value.Day;
  if not TryEncodeDate(Year, Month, Day, Result) then Result := Date;
end;

function DateTimeToRecord(Value: TDateTime): TDateRecord;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  Result.Year := Year;
  Result.Month := Month;
  Result.Day := Day;
end;

function IsEmpty(const Value: TDateRecord): Boolean;
begin
  Result := (Value.Year = 0) and (Value.Month = 0) and (Value.Day = 0);
end;

function GetStartDate(const Value: TDateRecord): TDateTime;
begin
  if Value.Year > 0 then
  begin
    if Value.Month = 0 then
      Result := StartOfTheYear(Value.Year)
    else
      if Value.Day = 0 then
        Result := StartOfAMonth(Value.Year, Value.Month)
      else
        Result := StartOfADay(Value.Year, Value.Month, Value.Day);
  end
  else
    Result := StartOfTheYear(Now);
end;

function GetEndDate(const Value: TDateRecord): TDateTime;
begin
  if Value.Year > 0 then
  begin
    if Value.Month = 0 then
      Result := EndOfTheYear(Value.Year)
    else
      if Value.Day = 0 then
        Result := EndOfAMonth(Value.Year, Value.Month)
      else
        Result := EndOfADay(Value.Year, Value.Month, Value.Day);
  end
  else
    Result := EndOfTheYear(Now);
end;

function DoValidIdent(const Value: string): string;
  function Alpha(C: Char): Boolean; inline;
  begin
    Result := TCharacter.IsLetter(C) or (C = '_');
  end;

  function AlphaNumeric(C: Char): Boolean; inline;
  begin
    Result := TCharacter.IsLetterOrDigit(C) or (C = '_');
  end;
var
  I: Integer;
begin
  if (Value <> '') and not IsValidIdent(Value) then
  begin
    I := 1;
    Result := '';

    while (I <= Length(Value)) do
    begin
      if Alpha(Value[I]) then
      begin
        Result := Value[I];
        Inc(I);
        Break;
      end
      else
        Inc(I);
    end;

    while (I <= Length(Value)) do
    begin
      if AlphaNumeric(Value[I]) then
      begin
        Result := Result + Value[I];
        Inc(I);
      end
      else
        Inc(I);
    end;
  end
  else
    Result := Value;
end;

function GetFileContent(const AFilename: string; AEncoding: TEncoding): string;
var
  Stream: TFileStream;
  Size: Integer;
  Buffer: TBytes;
  Encoding: TEncoding;
begin
  if FileExists(AFilename) then
  begin
    Stream := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
    try
      Size := Stream.Size;
      SetLength(Buffer, Size);
      Stream.Read(Buffer[0], Size);
      Encoding := nil;
      Size := TEncoding.GetBufferEncoding(Buffer, Encoding, AEncoding);
      Result := Encoding.GetString(Buffer, Size, Length(Buffer) - Size);
    finally
      Stream.Free;
    end;
  end
  else
    Result := '';
end;

initialization
  SmartyProvider := TSmartyInfoProvider.Create;

finalization
  SmartyProvider.Free;

end.

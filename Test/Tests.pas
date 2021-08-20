unit Tests;

{$IFDEF FPC}
  {$mode Delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  {$IFDEF FPC}
  fpcunit, testregistry, consoletestrunner { you can add units after this },
  {$ENDIF}
  {$IFDEF DCC}
  TestFramework,
  {$ENDIF}
  TemplateEngine;

type
  TTestCaseData = record
    Section, Template, ExpectResult: string;
  end;

  TTplTest = class(TTestCase)
  private
    class var
    FTotalAssertCount: Integer;     // count of all asserts
  protected
    procedure CheckTpl(const TCData: TTestCaseData);
    procedure CheckSection(const Section: string);
    {$IFDEF FPC}
    procedure RunTest; override;
    {$ENDIF}
  public
    class property TotalAssertCnt: Integer read FTotalAssertCount;
  published
    procedure Special;
    procedure Vars;
    procedure Mods;
    procedure Functions;
    procedure Control;
  end;

procedure LoadData;
procedure FinData;

implementation

type
  TVariableArrayItemArr = array of TVariableArrayItem;

// Source data and test cases. Should better be class members but Data is finalized
// before class d-tor thus causing mem leak.
var
  Data: TVariableArrayItemArr;   // source values for test
  TestCases: array of TTestCaseData; // test fixtures

// Load variables
procedure LoadData;

const
  QUOTE = Char('''');
  SEP_NO_SP = QUOTE + '=' + QUOTE;
  SEP_SP = QUOTE + ' = ' + QUOTE;

  function Unquote(const Str: string; Left, Right: Char): string;
  begin
    if (Length(Str) < 2) or ( (Str[1] <> Left) or (Str[Length(Str)] <> Right) ) then
      raise Exception.Create('Error unquoting ' + Str);
    Result := Copy(Str, 2, Length(Str) - 2);
  end;

  procedure SplitLine(const Str: string; out Template, ExpectResult: string);
  var SepPos, SepLen: Integer;
  begin
    // Try to split by both variants
    SepPos := Pos(SEP_NO_SP, Str);
    if SepPos <> 0 then
      SepLen := Length(SEP_NO_SP)
    else
    begin
      SepPos := Pos(SEP_SP, Str);
      if SepPos <> 0 then
        SepLen := Length(SEP_SP)
      else
        raise Exception.Create('Line not recognized' + Str);
    end;

    Template := Unquote(Trim(Copy(Str, 1, SepPos)), QUOTE, QUOTE);
    ExpectResult := Unquote(Trim(Copy(Str, SepPos + SepLen - 1, MaxInt)), QUOTE, QUOTE);
  end;

  // EncodeDate+EncodeTime produce "extended" result in-place; TDateTime typecast
  // is not allowed, so we need typecast via variable or function
  function EncodeDateTime(Year, Month, Day, Hour, Min, Sec, MSec: Word): TDateTime;
  begin
    Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Min, Sec, MSec);
  end;

var
  sl: TStringList;
  s, CurrSection: string;
  TCData: TTestCaseData;
begin
  Data := TVariableArrayItemArr.Create(
    Item('bool', True),
    Item('float', 10.5),
    Item('int', 10),
    Item('str', 'string'),
    Item('datetime', EncodeDateTime(2000, 1, 2, 3, 4, 5, 6)),
    Item('arr', ['Sub1', 'Sub2']),
    Item('map', [
      Item('field1', 10),
      Item('field2', 'foo')
    ])
  );

  sl := TStringList.Create;
  sl.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'TestCases.lst');
  CurrSection := '';
  for s in sl do
  begin
    if (s = '') then Continue;
    case s[1] of
      '[':
        CurrSection := Unquote(s, '[', ']');
      QUOTE:
        begin
          TCData.Section := CurrSection;
          SplitLine(s, TCData.Template, TCData.ExpectResult);
          SetLength(TestCases, Length(TestCases) + 1);
          TestCases[High(TestCases)] := TCData;
        end;
    end;
  end;
  FreeAndNil(sl);
end;

// TVariableRecord is not auto-destructible so finalize data manually
procedure FinData;
var i: Integer;
begin
  for i := Low(Data) to High(Data) do
    Data[i].Item.Finalize;
end;

procedure TTplTest.CheckTpl(const TCData: TTestCaseData);
var Actual, Errors: string;
begin
  CheckTrue(SmartyExec(TCData.Template, 'test', Data, Errors, Actual), 'Exec is true, template ' + TCData.Template);
  CheckEquals(Errors, '', 'Errors are empty, template ' + TCData.Template);
  CheckEquals(TCData.ExpectResult, Actual, 'Results are equal, template ' + TCData.Template);
  {$IFDEF DCC}
  Inc(FTotalAssertCount, 3); // Primitive implementation of total assert counter
  {$ENDIF}
end;

procedure TTplTest.CheckSection(const Section: string);
var data: TTestCaseData;
begin
  for data in TestCases do
    if data.Section = Section then
      CheckTpl(data);
end;

{$IFDEF FPC}
// AssertCount is zeroed before each test so override the method to inc total assert counter
procedure TTplTest.RunTest;
begin
  inherited RunTest;
  Inc(FTotalAssertCount, AssertCount);
end;
{$ENDIF}

procedure TTplTest.Special;
begin
  CheckSection('special');
end;

procedure TTplTest.Vars;
begin
  CheckSection('vars');
end;

procedure TTplTest.Mods;
begin
  CheckSection('mods');
end;

procedure TTplTest.Functions;
begin
  CheckSection('functions');
end;

procedure TTplTest.Control;
begin
  CheckSection('control');
end;

initialization
  LoadData;
  {$IFDEF DCC}
  RegisterTest(TTplTest.Suite);
  {$ENDIF}
  {$IFDEF FPC}
  RegisterTest(TTplTest);
  {$ENDIF}
finalization
  FinData;
end.

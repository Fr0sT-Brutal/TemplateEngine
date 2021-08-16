{ *********************************************************************** }
{                                                                         }
{ DVD Chief Template Engine                                               }
{ Smarty-like template processing Engine                                  }
{                                                                         }
{ Copyright (c) 2013 Adit Software http://dvdchief.com/delphi             }
{                                                                         }
{ *********************************************************************** }

unit frmDemo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  TemplateEngine;

type
  TForm1 = class(TForm)
    memParse: TMemo;
    spl: TSplitter;
    Parse: TButton;
    Panel1: TPanel;
    memResult: TMemo;
    chbNewMethod: TCheckBox;
    chbStripLineBr: TCheckBox;
    procedure ParseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TMyNamespace = class (TNamespaceProvider)
  public
    function GetName: string; override;     //Get Namespace Name
    function IsIndexSupported: boolean; override;
    function UseCache: boolean; override;
    procedure GetIndexProperties(var AMin, AMax: integer); override;
    function GetVariable(AIndex: integer;
    	const AVarName: string): TVariableRecord; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{************* TMyNamespace *************}

function TMyNamespace.GetName: string;
begin
	Result := 'mynamespace';
end;

function TMyNamespace.IsIndexSupported: boolean;
begin
	Result := false;
end;

function TMyNamespace.UseCache: boolean;
begin
	Result := true;
end;

procedure TMyNamespace.GetIndexProperties(var AMin, AMax: integer);
begin
  AMin := 0;
  AMax := 0;
end;

function TMyNamespace.GetVariable(AIndex: integer;
	const AVarName: string): TVariableRecord;
begin
  if CompareText(AVarName, 'string') = 0 then Result := 'string'
  else if CompareText(AVarName, 'int') = 0 then Result := 10
  else if CompareText(AVarName, 'float') = 0 then Result := 20.0
  else if CompareText(AVarName, 'bool') = 0 then Result := false
  else if CompareText(AVarName, 'array') = 0 then
  begin
    Result.SetArrayLength(4);
    Result.SetArrayItemQ(0, 'str', 'test string');
    Result.SetArrayItemQ(1, 'int', 29);
    Result.SetArrayItemQ(2, 'float', 2.0);
    Result.SetArrayItemQ(3, 'bool', true);
  end
  else
    Result := TVariableRecord.Null;
end;

procedure TForm1.ParseClick(Sender: TObject);
var
  Smarty: TSmartyEngine;
  Errors: TStringList;
  Namesp: TStorageNamespaceProvider;
begin
  if not chbNewMethod.Checked then
  begin
    Smarty := TSmartyEngine.Create;
    Errors := TStringList.Create;
    try
      Smarty.AddNamespace(TMyNamespace.Create); //adding needed namespace
      if Smarty.Compile(memParse.Lines.Text, Errors) then
        memResult.Lines.Text := Smarty.Execute;

    finally
      Errors.Free;
      Smarty.Free;
    end;
  end
  else
  begin
    Smarty := TSmartyEngine.Create;
    Smarty.StripLineBreaksAfterBlocks := chbStripLineBr.Checked;
    Errors := TStringList.Create;
    Namesp := TStorageNamespaceProvider.Create('mynamespace');
    Namesp.SetVariable('string', 'string');
    Namesp.SetVariable('int', 10);
    Namesp.SetVariable('float', 20.0);
    Namesp.SetVariable('bool', false);
    Namesp.SetVariable('array', ['test string', 29, 2.0, true]);
    Namesp.SetVariable('nested_array',
      Arr([
        Map([
          Item('name', 'Foo'),
          Item('subitems', ['Sub1', 'Sub2'])
        ]),
        Map([
          Item('name', 'Bar'),
          Item('subitems', ['Sub1', 'Sub2'])
        ])
      ])
    );
    try
      Smarty.AddNamespace(Namesp); //adding needed namespace
      if Smarty.Compile(memParse.Lines.Text, Errors) then
        memResult.Lines.Text := Smarty.Execute;
    finally
      Errors.Free;
      Smarty.Free;
    end;
  end;
end;

end.

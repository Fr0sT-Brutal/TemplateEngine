{ *********************************************************************** }
{                                                                         }
{ DVD Chief Template Engine                                               }
{ Smarty-like template processing Engine                                  }
{                                                                         }
{ Copyright (c) 2013 Adit Software http://dvdchief.com/delphi             }
{                                                                         }
{ *********************************************************************** }

unit frmDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls, OleCtrls, SHDocVw,
  TemplateEngine;

type
  TForm1 = class(TForm)
    memParse: TMemo;
    spl: TSplitter;
    Parse: TButton;
    Panel1: TPanel;
    memResult: TMemo;
    procedure ParseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TMyNamespace = class (TNamespaceProvider)
  public
    class function GetName: string; override;     //Get Namespace Name
    class function IsIndexSupported: boolean; override;
    class function UseCache: boolean; override;
    procedure GetIndexProperties(var AMin, AMax: integer); override;
    function GetVariable(AIndex: integer;
    	AVarName: string): TVariableRecord; override;
  end;

  TMyStorageNamespace = class(tStorageNamespaceProvider)
    class function GetName: string; override;     //Get Namespace Name
    class function IsIndexSupported: boolean; override;
    class function UseCache: boolean; override;
    procedure GetIndexProperties(var AMin, AMax: integer); override;
  end;

var
  Form1: TForm1;
  UseStorage: Boolean = False;

implementation

{$R *.dfm}

{************* TMyNamespace *************}

class function TMyNamespace.GetName: string;
begin
	Result := 'mynamespace';
end;

class function TMyNamespace.IsIndexSupported: boolean;
begin
	Result := false;
end;

class function TMyNamespace.UseCache: boolean;
begin
	Result := true;
end;

procedure TMyNamespace.GetIndexProperties(var AMin, AMax: integer);
begin
  AMin := 0;
  AMax := 0;
end;

function TMyNamespace.GetVariable(AIndex: integer;
	AVarName: string): TVariableRecord;
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

{************* TMyStorageNamespace *************}

class function TMyStorageNamespace.GetName: string;
begin
	Result := 'mynamespace';
end;

class function TMyStorageNamespace.IsIndexSupported: boolean;
begin
	Result := false;
end;

class function TMyStorageNamespace.UseCache: boolean;
begin
	Result := true;
end;

procedure TMyStorageNamespace.GetIndexProperties(var AMin, AMax: integer);
begin
  AMin := 0;
  AMax := 0;
end;

procedure TForm1.ParseClick(Sender: TObject);
var
  Smarty: TSmartyEngine;
  Errors: TStringList;
  Namesp: TMyStorageNamespace;
begin
  if not UseStorage then
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
    Errors := TStringList.Create;
    Namesp := TMyStorageNamespace.Create;
    Namesp.SetVariable('string', 'string');
    Namesp.SetVariable('int', 10);
    Namesp.SetVariable('float', 20.0);
    Namesp.SetVariable('bool', false);
    Namesp.SetVariable('array', ['test string', 29, 2.0, true]);
    try
      Smarty.AddNamespace(Namesp); //adding needed namespace
      if Smarty.Compile(memParse.Lines.Text, Errors) then
        memResult.Lines.Text := Smarty.Execute;
    finally
      Errors.Free;
      Smarty.Free;
    end;
  end;
  UseStorage := not UseStorage;
end;

end.

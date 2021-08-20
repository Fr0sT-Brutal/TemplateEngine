program Demo;

uses
  Forms,
  {$IFDEF FPC}
    Interfaces,
  {$ENDIF}
  frmDemo in 'frmDemo.pas' {Form1};

{$R *.res}

begin
  {$IFDEF FPC}
  Application.Scaled := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

program UnitTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  {$IFDEF FPC}
  fpcunit, testregistry, consoletestrunner { you can add units after this },
  {$ENDIF}
  {$IFDEF DCC}
  TextTestRunner,
  {$ENDIF}
  Tests;

{$IFDEF FPC}
var
  App: TTestRunner;
{$ENDIF}
{$IFDEF DCC}
var
  Res: TObject;
{$ENDIF}

begin
  {$IFDEF DCC}
  ReportMemoryLeaksOnShutdown := True;
  // RunRegisteredTests returns result object that holds reference to the whole test suite
  // so lots of stuff won't be autodestroyed without freeing the result
  Res := TextTestRunner.RunRegisteredTests(rxbHaltOnFailures);
  FreeAndNil(Res);
  {$ENDIF}

  {$IFDEF FPC}
  // Provide no-arguments launch
  DefaultRunAllTests := True;
  DefaultFormat := fPlain;
  App := TTestRunner.Create(nil);
  App.Initialize;
  App.Run;
  App.Free;
  {$ENDIF}

  Writeln('Asserts: ', TTplTest.TotalAssertCnt);
  Sleep(1000);
end.

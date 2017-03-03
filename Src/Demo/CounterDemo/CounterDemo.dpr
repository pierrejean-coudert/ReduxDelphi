program CounterDemo;

uses
  Vcl.Forms,
  DemoCounterForm in 'DemoCounterForm.pas' {FormDemoCounter};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormDemoCounter, FormDemoCounter);
  Application.Run;
end.

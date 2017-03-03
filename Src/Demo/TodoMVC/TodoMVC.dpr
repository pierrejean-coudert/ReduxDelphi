program TodoMVC;

uses
  Vcl.Forms,
  TodoMVCForm in 'TodoMVCForm.pas' {Form2},
  TodoActions in 'TodoActions.pas',
  TodoStates in 'TodoStates.pas',
  TodoReducer in 'TodoReducer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

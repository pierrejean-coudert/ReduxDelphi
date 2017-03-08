program TodoMVC;

uses
  Vcl.Forms,
  TodoMVCForm in 'TodoMVCForm.pas' {FormTodo},
  TodoActions in 'TodoActions.pas',
  TodoStates in 'TodoStates.pas',
  TodoReducer in 'TodoReducer.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormTodo, FormTodo);
  Application.Run;
end.

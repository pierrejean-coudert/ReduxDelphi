unit TodoMVCForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  System.Generics.Collections,
  Redux,
  StdAction,
  TodoStates,
  TodoActions,
  TodoReducer, Vcl.StdCtrls, Vcl.CheckLst;

type

  TForm2 = class(TForm)
    PanelHeaner: TPanel;
    PanelTodos: TPanel;
    PanelFooter: TPanel;
    CheckBoxAll: TCheckBox;
    CheckListBoxTodo: TCheckListBox;
    EditTodo: TEdit;
    LabelTitle: TLabel;
    procedure FormShow(Sender: TObject);
    procedure EditTodoKeyPress(Sender: TObject; var Key: Char);
    procedure CheckListBoxTodoClickCheck(Sender: TObject);

  private
    initialState: TApplicationState;
    FGUIDList : TList<TGUID>;
    FStore : TStore<TApplicationState, IAction>;

  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.CheckListBoxTodoClickCheck(Sender: TObject);
var
  index : Integer;
  id : TGUID;
begin
  index := CheckListBoxTodo.ItemIndex;
  id := FGUIDList[index];
  FStore.Dispatch(TCompleteTodoAction.Create(id));
end;

procedure TForm2.EditTodoKeyPress(Sender: TObject; var Key: Char);
begin
   if Key = #13 then begin
     FStore.Dispatch(TAddTodoAction.Create(EditTodo.Text));
     EditTodo.Text := '';
   end;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  FGUIDList:= TList<TGUID>.Create;

  initialState := TApplicationState.Create();
  FStore := TStore<TApplicationState, IAction>.Create(ApplicationReducer, initialState);
	FStore.subscribe( procedure (State: TApplicationState)
    var
      todo: TTodo;
      index : Integer;
    begin
      CheckListBoxTodo.Items.Clear;
      FGUIDList.Clear;
      for todo in State.Todos do begin
        index := CheckListBoxTodo.Items.Add(todo.Text);
        FGUIDList.Add(todo.Id);
        CheckListBoxTodo.Checked[index] := todo.IsCompleted;
      end;

    end
  );

  FStore.Dispatch(TActionInit.Create());
end;

end.

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
  TodoReducer, Vcl.StdCtrls, Vcl.CheckLst, Vcl.Menus;

type

  TForm2 = class(TForm)
    PanelHeaner: TPanel;
    PanelTodos: TPanel;
    PanelFooter: TPanel;
    CheckBoxAll: TCheckBox;
    CheckListBoxTodo: TCheckListBox;
    EditTodo: TEdit;
    LabelTitle: TLabel;
    LabelLeft: TLabel;
    LabelFilterAll: TLabel;
    LabelFilterActive: TLabel;
    LabelFilterCompleted: TLabel;
    LabelClearCompleted: TLabel;
    PopupMenuDelete: TPopupMenu;
    MenuItemDelete: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure EditTodoKeyPress(Sender: TObject; var Key: Char);
    procedure CheckListBoxTodoClickCheck(Sender: TObject);
    procedure LabelFilterAllClick(Sender: TObject);
    procedure LabelFilterActiveClick(Sender: TObject);
    procedure LabelFilterCompletedClick(Sender: TObject);
    procedure LabelClearCompletedClick(Sender: TObject);
    procedure CheckBoxAllClick(Sender: TObject);
    procedure MenuItemDeleteClick(Sender: TObject);

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

procedure TForm2.CheckBoxAllClick(Sender: TObject);
begin
  FStore.Dispatch(TCompleteAllTodosAction.Create(CheckBoxAll.Checked));
end;

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

procedure TForm2.LabelClearCompletedClick(Sender: TObject);
begin
  FStore.Dispatch(TClearCompletedTodosAction.Create);
end;

procedure TForm2.LabelFilterActiveClick(Sender: TObject);
begin
  FStore.Dispatch(TFilterTodosAction.Create(InProgress));
end;

procedure TForm2.LabelFilterAllClick(Sender: TObject);
begin
  FStore.Dispatch(TFilterTodosAction.Create(All));
end;

procedure TForm2.LabelFilterCompletedClick(Sender: TObject);
begin
  FStore.Dispatch(TFilterTodosAction.Create(Completed));
end;

procedure TForm2.MenuItemDeleteClick(Sender: TObject);
var
  I: Integer;
  id : TGUID;
begin
  for I := 0 to CheckListBoxTodo.Count-1 do
    if  CheckListBoxTodo.Selected[I] then begin
      id := FGUIDList[I];
      FStore.Dispatch(TDeleteTodoAction.Create(id));
      Break;
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
      // updtae list
      CheckListBoxTodo.Items.Clear;
      FGUIDList.Clear;
      for todo in State.Todos do begin
        if (State.Filter= All)
            or ((State.Filter= InProgress) and (not todo.IsCompleted))
            or  ((State.Filter= Completed) and ( todo.IsCompleted)) then
        begin
          index := CheckListBoxTodo.Items.Add(todo.Text);
          FGUIDList.Add(todo.Id);
          CheckListBoxTodo.Checked[index] := todo.IsCompleted;
        end;
      end;
      LabelLeft.Caption := format('%d item(s) left',[State.ItemLeftCount]);

      // update filter
      LabelFilterAll.Font.Style := [];
      LabelFilterActive.Font.Style := [];
      LabelFilterCompleted.Font.Style := [];
      case State.Filter of
        All: LabelFilterAll.Font.Style := [fsBold];
        InProgress: LabelFilterActive.Font.Style := [fsBold];
        Completed: LabelFilterCompleted.Font.Style := [fsBold];
      end;

    end
  );

  FStore.Dispatch(TActionInit.Create());
end;

end.

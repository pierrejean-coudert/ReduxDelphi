unit TodoReducer;

interface
uses
  System.Generics.Collections,
  System.SysUtils,
  StdAction,
  TodoActions,
  TodoStates;

  function ApplicationReducer(State: TApplicationState; Action: IAction): TApplicationState;

implementation

  function AddTodoReducer( State: TList<TTodo>; Action: TAddTodoAction ): TList<TTodo>;
  var
    AGUID : TGUID;
    ATodo : TTodo;
  begin
    CreateGUID(ATodo.Id);
    ATodo.Text := Action.Text;
    ATodo.IsCompleted := False;
    // TODO: should copy State before (immutability)
    State.Insert(0, ATodo);
    Result := State;
  end;

  function ClearCompletedTodosReducer( State: TList<TTodo>; Action: TClearCompletedTodosAction ): TList<TTodo>;
  begin
    //Result := State.RemoveAll(todo => todo.IsCompleted);
  end;

  function CompleteAllTodosReducer( State: TList<TTodo>; Action: TCompleteAllTodosAction ): TList<TTodo>;
  begin
//    Result := State
//          .Select(x => new Todo
//          {
//              Id = x.Id,
//              Text = x.Text,
//              IsCompleted = action.IsCompleted
//          })
//          .ToImmutableArray();
  end;

  function CompleteTodoReducer( State: TList<TTodo>; Action: TCompleteTodoAction ): TList<TTodo>;
  begin
//      var todoToEdit = State.First(todo => todo.Id == action.TodoId);
//
//      Result := State
//          .Replace(todoToEdit, new Todo
//          {
//              Id = todoToEdit.Id,
//              Text = todoToEdit.Text,
//              IsCompleted = !todoToEdit.IsCompleted
//          });
  end;

  function DeleteTodoReducer( State: TList<TTodo>; Action:  TDeleteTodoAction ): TList<TTodo>;
  begin
//      var todoToDelete = previousState.First(todo => todo.Id == action.TodoId);
//
//      return previousState.Remove(todoToDelete);
  end;

  function TodosReducer(State: TList<TTodo>; Action: IAction): TList<TTodo>;
  begin
    if (action is TAddTodoAction) then
        Result := AddTodoReducer(State, TAddTodoAction(Action))

    else if (action is TClearCompletedTodosAction) then
        Result :=  ClearCompletedTodosReducer(State, TClearCompletedTodosAction(Action))

    else if (action is TCompleteAllTodosAction) then
        Result :=  CompleteAllTodosReducer(State, TCompleteAllTodosAction(Action))

    else if (action is TCompleteTodoAction) then
        Result :=  CompleteTodoReducer(State, TCompleteTodoAction(Action))

    else if (action is TDeleteTodoAction) then
        Result :=  DeleteTodoReducer(State, TDeleteTodoAction(Action))
    else
        result := State;

  end;

  function FilterReducer(State: TTodosFilter; Action: IAction): TTodosFilter;
  begin
    if Action is TFilterTodosAction  then
      Result :=  (Action as TFilterTodosAction).Filter
    else
      Result := State;
  end;

  function ApplicationReducer(State: TApplicationState; Action: IAction): TApplicationState;
  begin
    Result := TApplicationState.Create(
      TodosReducer(State.Todos, action),
      FilterReducer(State.Filter, action)
    );
  end;
end.

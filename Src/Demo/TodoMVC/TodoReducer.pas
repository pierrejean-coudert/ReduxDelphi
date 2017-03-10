unit TodoReducer;

interface
uses
  System.Generics.Collections,
  System.SysUtils,
  StdAction,
  TodoActions,
  TodoStates;

  function ApplicationReducer(State: IApplicationState; Action: IAction): IApplicationState;

implementation

  function AddTodoReducer( State: ITodoList; Action: TAddTodoAction ): ITodoList;
  var
    AGUID : TGUID;
    ATodo : ITodo;
  begin
    CreateGUID(AGUID);
    ATodo := TTodo.Create(Action.Text, False, AGUID);
    Result := State.Insert(0, ATodo);
  end;

  function ClearCompletedTodosReducer( State: ITodoList; Action: TClearCompletedTodosAction ): ITodoList;
  begin
    Result := State.Filter( function(Todo : ITodo):Boolean
                begin
                  Result :=  not Todo.IsCompleted;
                end);
  end;

  function CompleteAllTodosReducer( State: ITodoList; Action: TCompleteAllTodosAction ): ITodoList;
  begin
    Result := State.Map( function(Todo : ITodo):ITodo
                begin
                  Result :=  TTodo.Create(Todo.Text, Action.Value, Todo.Id);
                end);
  end;

  function CompleteTodoReducer( State: ITodoList; Action: TCompleteTodoAction ): ITodoList;
  begin
    Result := State.Map( function(Todo : ITodo):ITodo
                begin
                  if Todo.Id = Action.GUID then
                    Result := TTodo.Create(Todo.Text, not Todo.IsCompleted, Todo.Id)
                  else
                    Result := Todo;
                end);
  end;


  function DeleteTodoReducer( State: ITodoList; Action:  TDeleteTodoAction ): ITodoList;
  begin
    Result := State.Filter( function(Todo : ITodo):Boolean
                begin
                  Result :=  Todo.Id <> Action.GUID;
                end);
  end;


  function TodosReducer(State: ITodoList; Action: IAction): ITodoList;
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

  function ApplicationReducer(State: IApplicationState; Action: IAction): IApplicationState;
  begin
    Result := TApplicationState.Create(
      TodosReducer(State.Todos, Action),
      FilterReducer(State.Filter, Action)
    );
  end;
end.

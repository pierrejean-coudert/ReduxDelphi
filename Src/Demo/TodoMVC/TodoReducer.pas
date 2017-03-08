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

  function AddTodoReducer( State: TList<ITodo>; Action: TAddTodoAction ): TList<ITodo>;
  var
    AGUID : TGUID;
    ATodo : TTodo;
    NewState :  TList<ITodo>;
  begin
    CreateGUID(AGUID);
    ATodo := TTodo.Create(Action.Text, False, AGUID);
    NewState := TList<ITodo>.Create(State);
    NewState.Insert(0, ATodo);
    Result := NewState;
  end;

  function ClearCompletedTodosReducer( State: TList<ITodo>; Action: TClearCompletedTodosAction ): TList<ITodo>;
  var
    Todo : ITodo;
    NewState :  TList<ITodo>;
  begin
    NewState := TList<ITodo>.Create();
    for Todo in State do begin
      if not Todo.IsCompleted then
        NewState.Add(Todo)
    end;
    Result := NewState;
  end;

  function CompleteAllTodosReducer( State: TList<ITodo>; Action: TCompleteAllTodosAction ): TList<ITodo>;
  var
    Todo, newTodo : ITodo;
    NewState :  TList<ITodo>;
  begin
    NewState := TList<ITodo>.Create();
    for Todo in State do begin
      NewTodo := TTodo.Create(Todo.Text, Action.Value, Todo.Id);
      NewState.Add(NewTodo)
    end;
    Result := NewState;
  end;


  function CompleteTodoReducer( State: TList<ITodo>; Action: TCompleteTodoAction ): TList<ITodo>;
  var
    Todo, newTodo : ITodo;
    NewState :  TList<ITodo>;
  begin
    NewState := TList<ITodo>.Create();
    for Todo in State do begin
      if Todo.Id = Action.GUID then begin
        NewTodo := TTodo.Create(Todo.Text, not Todo.IsCompleted, Todo.Id);
      end
      else begin
        NewTodo := TTodo.Create(Todo);
      end;
      NewState.Add(NewTodo)
    end;
    Result := NewState;
  end;


  function DeleteTodoReducer( State: TList<ITodo>; Action:  TDeleteTodoAction ): TList<ITodo>;
  var
    Todo : ITodo;
    NewState :  TList<ITodo>;
  begin
    NewState := TList<ITodo>.Create();
    for Todo in State do begin
      if Todo.Id <> Action.GUID then
        NewState.Add(Todo)
    end;
    Result := NewState;
  end;


  function TodosReducer(State: TList<ITodo>; Action: IAction): TList<ITodo>;
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
      TodosReducer(State.GetTodos, Action),
      FilterReducer(State.GetFilter, Action)
    );
  end;
end.

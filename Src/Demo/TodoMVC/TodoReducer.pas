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
    ATodo : TTodo;
    NewState :  TList<TTodo>;
  begin
    CreateGUID(ATodo.Id);
    ATodo.Text := Action.Text;
    ATodo.IsCompleted := False;
    NewState := TList<TTodo>.Create(State);
    NewState.Insert(0, ATodo);
    Result := NewState;
  end;

  function ClearCompletedTodosReducer( State: TList<TTodo>; Action: TClearCompletedTodosAction ): TList<TTodo>;
  var
    Todo : TTodo;
    NewState :  TList<TTodo>;
  begin
    NewState := TList<TTodo>.Create();
    for Todo in State do begin
      if not Todo.IsCompleted then
        NewState.Add(Todo)
    end;
    Result := NewState;
  end;

  function CompleteAllTodosReducer( State: TList<TTodo>; Action: TCompleteAllTodosAction ): TList<TTodo>;
  var
    Todo : TTodo;
    NewState :  TList<TTodo>;
  begin
    NewState := TList<TTodo>.Create();
    for Todo in State do begin
      //Todo.IsCompleted := Action.Value;
      NewState.Add(Todo)
    end;
    Result := NewState;
  end;


  function CompleteTodoReducer( State: TList<TTodo>; Action: TCompleteTodoAction ): TList<TTodo>;
  var
    Todo : TTodo;
    newTodo  : TTodo;
    NewState :  TList<TTodo>;
  begin
    NewState := TList<TTodo>.Create();
    for Todo in State do begin
      if Todo.Id = Action.GUID then
        NewTodo := Todo;
        NewTodo.IsCompleted := not Todo.IsCompleted;
      NewState.Add(NewTodo)
    end;
    Result := NewState;
  end;


  function DeleteTodoReducer( State: TList<TTodo>; Action:  TDeleteTodoAction ): TList<TTodo>;
  var
    Todo : TTodo;
    NewState :  TList<TTodo>;
  begin
    NewState := TList<TTodo>.Create();
    for Todo in State do begin
      if Todo.Id <> Action.GUID then
        NewState.Add(Todo)
    end;
    Result := NewState;
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

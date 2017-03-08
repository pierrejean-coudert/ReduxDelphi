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
    NewState :  ITodoList;
  begin
    CreateGUID(AGUID);
    ATodo := TTodo.Create(Action.Text, False, AGUID);
    NewState := TTodoList.Create(State);
    NewState.Insert(0, ATodo);
    Result := NewState;
  end;

  function ClearCompletedTodosReducer( State: ITodoList; Action: TClearCompletedTodosAction ): ITodoList;
  var
    Todo : ITodo;
    NewState :  ITodoList;
  begin
    NewState := TTodoList.Create();
    for Todo in State do begin
      if not Todo.IsCompleted then
        NewState.Add(Todo)
    end;
    Result := NewState;
  end;

  function CompleteAllTodosReducer( State: ITodoList; Action: TCompleteAllTodosAction ): ITodoList;
  var
    Todo, newTodo : ITodo;
    NewState : ITodoList;
  begin
    NewState := TTodoList.Create();
    for Todo in State do begin
      NewTodo := TTodo.Create(Todo.Text, Action.Value, Todo.Id);
      NewState.Add(NewTodo)
    end;
    Result := NewState;
  end;


  function CompleteTodoReducer( State: ITodoList; Action: TCompleteTodoAction ): ITodoList;
  var
    Todo, newTodo : ITodo;
    NewState :  ITodoList;
  begin
    NewState := TTodoList.Create();
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


  function DeleteTodoReducer( State: ITodoList; Action:  TDeleteTodoAction ): ITodoList;
  var
    Todo : ITodo;
    NewState : ITodoList;
  begin
    NewState := TTodoList.Create();
    for Todo in State do begin
      if Todo.Id <> Action.GUID then
        NewState.Add(Todo)
    end;
    Result := NewState;
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
      TodosReducer(State.GetTodos, Action),
      FilterReducer(State.GetFilter, Action)
    );
  end;
end.

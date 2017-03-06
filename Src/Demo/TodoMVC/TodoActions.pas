unit TodoActions;

interface

uses
  StdAction,
  TodoStates;

type
  TAddTodoAction = class(TActionSetText);

  TDeleteTodoAction = class(TActionSetGUID);

  TCompleteTodoAction = class(TActionSetGUID);

  TCompleteAllTodosAction = class(TActionSetBool);

  TClearCompletedTodosAction = class(TActionInit);

  TFilterTodosAction= class abstract(TInterfacedObject, IAction)
    Filter : TTodosFilter;

    constructor Create(AFilter: TTodosFilter);
  end;

implementation

{ TFilterTodosAction }

constructor TFilterTodosAction.Create(AFilter: TTodosFilter);
begin
  Filter := AFilter;
end;

end.

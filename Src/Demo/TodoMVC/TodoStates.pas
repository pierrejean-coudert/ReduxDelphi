unit TodoStates;

interface

uses
   System.Generics.Collections,
   Immutable;

type
  TTodosFilter = (All, InProgress, Completed);

  ITodo = interface
    function Text: string;
    function IsCompleted : Boolean;
    function Id: TGUID;
  end;

  ITodoList = IImmutableList<ITodo>;

  IApplicationState = interface
    function ItemLeftCount : Integer;
    function Todos: ITodoList;
    function Filter : TTodosFilter;
  end;

  TTodo = class(TInterfacedObject, ITodo)
  private
    FText: string;
    FIsCompleted : Boolean;
    FId: TGUID;

  public
    constructor Create(ATodo: ITodo); overload;
    constructor Create(AText: string; AIsCompleted : Boolean; AId: TGUID); overload;

    function Text: string;
    function IsCompleted : Boolean;
    function Id: TGUID;
  end;

  TTodoList = TImmutableList<ITodo>;

  TApplicationState = class(TInterfacedObject, IApplicationState)
  private
    FTodos: ITodoList;
    FFilter: TTodosFilter;
  public
    constructor Create; overload;
    constructor Create(ATodos: ITodoList; AFilter: TTodosFilter); overload;

    function Todos: ITodoList;
    function Filter : TTodosFilter;
    function ItemLeftCount : Integer;
  end;

implementation

{ TTodo }

constructor TTodo.Create(ATodo: ITodo);
begin
  FText:= ATodo.Text;
  FIsCompleted:= ATodo.IsCompleted;
  FId:= ATodo.Id;
end;

constructor TTodo.Create(AText: string; AIsCompleted: Boolean; AId: TGUID);
begin
  FText:= AText;
  FIsCompleted:= AIsCompleted;
  FId:= AId;
end;

function TTodo.Id: TGUID;
begin
  Result := FId;
end;

function TTodo.IsCompleted: Boolean;
begin
  Result := FIsCompleted;
end;

function TTodo.Text: string;
begin
  Result := FText;
end;

{ TApplicationState }

constructor TApplicationState.Create;
begin
  FTodos :=  TTodoList.Create;
  FFilter := All;
end;

constructor TApplicationState.Create(ATodos: ITodoList; AFilter: TTodosFilter);
begin
  FTodos := TTodoList.Create(ATodos);
  FFilter := AFilter;
end;

function TApplicationState.Filter: TTodosFilter;
begin
  Result := FFilter;
end;

function TApplicationState.Todos: ITodoList;
begin
  Result := FTodos;
end;

function TApplicationState.ItemLeftCount: Integer;
var
  ATodo: ITodo;
begin
  Result := 0;
  for ATodo in FTodos do begin
    if not ATodo.IsCompleted then
      Inc(Result);
  end;
end;

end.

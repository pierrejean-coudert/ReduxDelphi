unit TodoStates;

interface

uses
   System.Generics.Collections;

type
  TTodosFilter = (All, InProgress, Completed);

  TTodo = class(TInterfacedObject)
    Text: string;
    IsCompleted : Boolean;
    Id: TGUID;

    constructor Create(ATodo: TTodo); overload;
  end;

  TApplicationState = class(TInterfacedObject)
    Todos: TList<TTodo>;
    Filter: TTodosFilter;

    constructor Create; overload;
    constructor Create(ATodos: TList<TTodo>; AFilter: TTodosFilter); overload;

    function ItemLeftCount : Integer;
  end;

implementation

{ TTodo }

constructor TTodo.Create(ATodo: TTodo);
begin
    Text:= ATodo.Text;
    IsCompleted:= ATodo.IsCompleted;
    Id:= ATodo.Id;
end;

{ TApplicationState }

constructor TApplicationState.Create;
begin
  Todos :=  TList<TTodo>.Create;
  Filter := All;
end;

constructor TApplicationState.Create(ATodos: TList<TTodo>;
  AFilter: TTodosFilter);
var
  ATodo, newTodo: TTodo;
begin
  Todos := TList<TTodo>.Create;
  for ATodo in ATodos do begin
    Todos.Add(TTodo.Create(ATodo));
  end;
  Todos :=  ATodos;
  Filter := AFilter;
end;

function TApplicationState.ItemLeftCount: Integer;
var
  ATodo: TTodo;
begin
  Result := 0;
  for ATodo in Todos do begin
    if not ATodo.IsCompleted then
      Inc(Result);
  end;
end;

end.

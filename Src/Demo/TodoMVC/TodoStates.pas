unit TodoStates;

interface

uses
   System.Generics.Collections;

type
  TTodosFilter = (All, InProgress, Completed);

  TTodo = record
    Text: string;
    IsCompleted : Boolean;
    Id: TGUID;
  end;

  TApplicationState = class(TInterfacedObject)
    Todos: TList<TTodo>;
    Filter: TTodosFilter;

    constructor Create; overload;
    constructor Create(ATodos: TList<TTodo>; AFilter: TTodosFilter); overload;
  end;

implementation

{ TApplicationState }

constructor TApplicationState.Create;
begin
  Todos :=  TList<TTodo>.Create;
  Filter := All;
end;

constructor TApplicationState.Create(ATodos: TList<TTodo>;
  AFilter: TTodosFilter);
begin
  Todos :=  ATodos;
  Filter := AFilter;
end;

end.

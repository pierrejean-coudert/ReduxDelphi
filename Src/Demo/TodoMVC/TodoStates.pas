unit TodoStates;

interface

uses
   System.Generics.Collections;

type
  TTodosFilter = (All, InProgress, Completed);

  ITodo = interface
    function Text: string;
    function IsCompleted : Boolean;
    function Id: TGUID;
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

  IApplicationState = interface
    function ItemLeftCount : Integer;
    function GetTodos: TList<ITodo>;
    function GetFilter : TTodosFilter;
  end;

  TApplicationState = class(TInterfacedObject, IApplicationState)
  private
    FTodos: TList<ITodo>;
    FFilter: TTodosFilter;
  public
    constructor Create; overload;
    constructor Create(ATodos: TList<ITodo>; AFilter: TTodosFilter); overload;
    destructor  Destroy; override;

    function GetTodos: TList<ITodo>;
    function GetFilter : TTodosFilter;
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
  FTodos :=  TList<ITodo>.Create;
  FFilter := All;
end;

constructor TApplicationState.Create(ATodos: TList<ITodo>;
  AFilter: TTodosFilter);
var
  ATodo: ITodo;
begin
  FTodos := TList<ITodo>.Create;
  for ATodo in ATodos do begin
    FTodos.Add(TTodo.Create(ATodo));
  end;
  FFilter := AFilter;
end;

destructor TApplicationState.Destroy;
begin
  FTodos.Clear;
  FTodos.Free;
  inherited;
end;

function TApplicationState.GetFilter: TTodosFilter;
begin
  Result := FFilter;
end;

function TApplicationState.GetTodos: TList<ITodo>;
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

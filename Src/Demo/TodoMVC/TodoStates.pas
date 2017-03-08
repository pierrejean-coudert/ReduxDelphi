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

  ITodoList = interface
    function Add(ATodo: ITodo): Integer;
    procedure Insert(Index: Integer; ATodo: ITodo);
    function GetEnumerator: TEnumerator<ITodo>;
  end;

  IApplicationState = interface
    function ItemLeftCount : Integer;
    function GetTodos: ITodoList;
    function GetFilter : TTodosFilter;
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

  TTodoList = class(TInterfacedObject, ITodoList)
  private
    FTodoList: TList<ITodo>;
  public
    constructor Create(); overload;
    constructor Create(ATodoList: ITodoList); overload;
    destructor  Destroy; override;

    function Add(ATodo: ITodo): Integer;
    procedure Insert(Index: Integer; ATodo: ITodo);
    function GetEnumerator: TEnumerator<ITodo>;
  end;

  TApplicationState = class(TInterfacedObject, IApplicationState)
  private
    FTodos: ITodoList;
    FFilter: TTodosFilter;
  public
    constructor Create; overload;
    constructor Create(ATodos: ITodoList; AFilter: TTodosFilter); overload;

    function GetTodos: ITodoList;
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

{ TTodoList }

constructor TTodoList.Create;
begin
  FTodoList :=  TList<ITodo>.Create;
end;

constructor TTodoList.Create(ATodoList: ITodoList);
var
  Todo : ITodo;
begin
  FTodoList :=  TList<ITodo>.Create;
  for Todo in ATodoList do
    FTodoList.Add(Todo);
end;

destructor TTodoList.Destroy;
begin
  FTodoList.Clear;
  FTodoList.Free;
  inherited;
end;

function TTodoList.GetEnumerator: TEnumerator<ITodo>;
begin
  Result := FTodoList.GetEnumerator;
end;

function TTodoList.Add(ATodo: ITodo): Integer;
begin
  Result := FTodoList.Add(ATodo);
end;

procedure TTodoList.Insert(Index: Integer; ATodo: ITodo);
begin
  FTodoList.Insert(Index, ATodo);
end;

{ TApplicationState }

constructor TApplicationState.Create;
begin
  FTodos :=  TTodoList.Create;
  FFilter := All;
end;

constructor TApplicationState.Create(ATodos: ITodoList; AFilter: TTodosFilter);
var
  ATodo: ITodo;
begin
  FTodos := TTodoList.Create;
  for ATodo in ATodos do begin
    FTodos.Add(TTodo.Create(ATodo));
  end;
  FFilter := AFilter;
end;

function TApplicationState.GetFilter: TTodosFilter;
begin
  Result := FFilter;
end;

function TApplicationState.GetTodos: ITodoList;
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

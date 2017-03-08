unit Redux;

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type

//TODo : add tests
//TODO : add multi-thread, async samples

  TSubscriber<TState> = reference to procedure (State: TState);

  TReducer<TState, TRdxAction> = reference to function (State: TState; Action: TRdxAction): TState;

  TDispatcher<TRdxAction> = reference to function(Action: TRdxAction): TRdxAction;
  TMiddleware<TRdxAction> = reference to function(Dispatcher: TDispatcher<TRdxAction>) : TDispatcher<TRdxAction>;

  IStore<TState, TRdxAction> = interface
    procedure Subscribe(Subscriber : TSubscriber<TState>);
    procedure Dispatch(Action: TRdxAction);
    procedure AddMiddleware(Middleware : TMiddleware<TRdxAction>);
    function  GetState(): TState;
  end;

  TStore<TState, TRdxAction> = class(TInterfacedObject, IStore<TState, TRdxAction>)
  private
    FState : TState;
    FReducer : TReducer<TState, TRdxAction>;
    FSubscriber : TList<TSubscriber<TState>>;
    FMiddlewares : TList<TMiddleware<TRdxAction>>;

    function innerDispatch(Action: TRdxAction): TRdxAction;
    function applyMiddleware(): TDispatcher<TRdxAction>;

  public
    constructor Create(Reducer: TReducer<TState, TRdxAction>; State:TState);
    destructor  Destroy; override;

    procedure Subscribe(Subscriber : TSubscriber<TState>);
    procedure Dispatch(Action: TRdxAction);
    procedure AddMiddleware(Middleware : TMiddleware<TRdxAction>);
    function  GetState(): TState;
  end;

implementation

{ TStore<TState, TRdxAction> }

function TStore<TState, TRdxAction>.applyMiddleware: TDispatcher<TRdxAction>;
var
  dispatcher : TDispatcher<TRdxAction>;
  middleware : TMiddleware<TRdxAction>;
begin
  dispatcher := innerDispatch;
 	for middleware in FMiddlewares do
		dispatcher := middleware(dispatcher);
  result := dispatcher;
end;

function TStore<TState, TRdxAction>.innerDispatch(Action: TRdxAction): TRdxAction;
var
  Subscriber : TSubscriber<TState>;
begin
  FState := FReducer(FState, Action);
 	for Subscriber in FSubscriber do
		Subscriber(FState);
end;

constructor TStore<TState, TRdxAction>.Create(Reducer: TReducer<TState, TRdxAction>;
                                           State: TState);
begin
  FState := State;
  FReducer := Reducer;
  FSubscriber := TList<TSubscriber<TState>>.Create();
  FMiddlewares := TList<TMiddleware<TRdxAction>>.Create();
end;

procedure TStore<TState, TRdxAction>.AddMiddleware(Middleware: TMiddleware<TRdxAction>);
begin
  FMiddlewares.Add(Middleware);
end;

destructor TStore<TState, TRdxAction>.Destroy;
begin
  FSubscriber.Clear;
  FMiddlewares.Clear;
  FSubscriber.Free;
  FMiddlewares.Free;
  inherited;
end;

procedure TStore<TState, TRdxAction>.Dispatch(Action: TRdxAction);
begin
  applyMiddleware()(Action);
end;

function TStore<TState, TRdxAction>.GetState: TState;
begin
  Result := FState;
end;

procedure TStore<TState, TRdxAction>.Subscribe(Subscriber : TSubscriber<TState>);
begin
  FSubscriber.Add(Subscriber);
end;

end.

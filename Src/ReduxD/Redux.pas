unit Redux;

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type

//TODo : add tests
//TODO : add TODOList sample
//TODO : add async sample

  TSubscriber<TState> = reference to procedure (State: TState);

  TReducer<TState, TAction> = reference to function (State: TState; Action: TAction): TState;

  TDispatcher<TAction> = reference to function(Action: TAction): TAction;
  TMiddleware<TAction> = reference to function(Dispatcher: TDispatcher<TAction>) : TDispatcher<TAction>;

  IStore<TState, TAction> = interface
    procedure Subscribe(Subscriber : TSubscriber<TState>);
    procedure Dispatch(Action: TAction);
    procedure AddMiddleware(Middleware : TMiddleware<TAction>);
    function  GetState(): TState;
  end;

  TStore<TState, TAction> = class(TInterfacedObject,IStore<TState, TAction>)
  private
    FState : TState;
    FReducer : TReducer<TState, TAction>;
    FSubscriber : TList<TSubscriber<TState>>;
    FMiddlewares : TList<TMiddleware<TAction>>;

    function innerDispatch(Action: TAction): TAction;
    function applyMiddleware(): TDispatcher<TAction>;

  public
    constructor Create(Reducer: TReducer<TState, TAction>; State:TState);

    procedure Subscribe(Subscriber : TSubscriber<TState>);
    procedure Dispatch(Action: TAction);
    procedure AddMiddleware(Middleware : TMiddleware<TAction>);
    function  GetState(): TState;
  end;

implementation

{ TStore<TState, TAction> }

function TStore<TState, TAction>.applyMiddleware: TDispatcher<TAction>;
var
  dispatcher : TDispatcher<TAction>;
  middleware : TMiddleware<TAction>;
begin
  dispatcher := innerDispatch;
 	for middleware in FMiddlewares do
		dispatcher := middleware(dispatcher);
  result := dispatcher;
end;

function TStore<TState, TAction>.innerDispatch(Action: TAction): TAction;
var
  Subscriber : TSubscriber<TState>;
begin
  FState := FReducer(FState, Action);
 	for Subscriber in FSubscriber do
		Subscriber(FState);
end;

constructor TStore<TState, TAction>.Create(Reducer: TReducer<TState, TAction>;
                                           State: TState);
begin
  FState := State;
  FReducer := Reducer;
  FSubscriber := TList<TSubscriber<TState>>.Create();
  FMiddlewares := TList<TMiddleware<TAction>>.Create();
end;

procedure TStore<TState, TAction>.AddMiddleware(Middleware: TMiddleware<TAction>);
begin
  FMiddlewares.Add(Middleware);
end;

procedure TStore<TState, TAction>.Dispatch(Action: TAction);
begin
  applyMiddleware()(Action);
end;

function TStore<TState, TAction>.GetState: TState;
begin
  Result := FState;
end;

procedure TStore<TState, TAction>.Subscribe(Subscriber : TSubscriber<TState>);
begin
  FSubscriber.Add(Subscriber);
end;

end.

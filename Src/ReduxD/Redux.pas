unit Redux;

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type

//TODO : add middlewares
//TODO : add TODO sample
//TODO : add async sample

  TSubscriber<TState> = reference to procedure (State: TState);

  TReducer<TState, TAction> = reference to function (State: TState; Action: TAction): TState;

//  TDispatcher<TAction> = reference to function(Action: TAction): TAction;
//  TStore<TState, TAction> = class;
  TMiddleware<TAction> = reference to function(Action: TAction) : TAction;

  TStore<TState, TAction> = class
  private
    FState : TState;
    FReducer : TReducer<TState, TAction>;
    FSubscriber : TList<TSubscriber<TState>>;
    FMiddlewares : TList<TMiddleware<TAction>>;

  public
      constructor Create(Reducer: TReducer<TState, TAction>; State:TState);

      procedure Subscribe(Subscriber : TSubscriber<TState>);
      procedure Dispatch(Action: TAction);
      procedure applyMiddleware(Middleware : TMiddleware<TAction>);
      function  GetState(): TState;
  end;

implementation

{ TStore<TState, TAction> }

constructor TStore<TState, TAction>.Create(Reducer: TReducer<TState, TAction>;
                                           State: TState);
begin
  FState := State;
  FReducer := Reducer;
  FSubscriber := TList<TSubscriber<TState>>.Create();
  FMiddlewares := TList<TMiddleware<TAction>>.Create();
end;

procedure TStore<TState, TAction>.applyMiddleware(Middleware: TMiddleware<TAction>);
begin
  FMiddlewares.Add(Middleware);
end;

procedure TStore<TState, TAction>.Dispatch(Action: TAction);
var
  Subscriber : TSubscriber<TState>;
begin
  FState := FReducer(FState, Action);
 	for Subscriber in FSubscriber do
		Subscriber(FState);
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

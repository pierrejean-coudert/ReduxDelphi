unit Logger;

interface

uses
  Redux,
  StdAction;

function LoggerMiddleware(Dispatcher: TDispatcher<IAction>) : TDispatcher<IAction>;

implementation

uses
  System.SysUtils,
  System.TypInfo,
  System.Rtti,
  Windows;

function GetRttiFromInterface(AIntf: IInterface): String;
var
  obj: TObject;
  ctx: TRttiContext;
begin
  obj := AIntf as TObject;
  Result := ctx.GetType(obj.ClassType).ToString;
end;

function LoggerMiddleware(Dispatcher: TDispatcher<IAction>) : TDispatcher<IAction>;
begin
  Result := function(Action : IAction):IAction
  begin
    OutputDebugString(PWideChar(GetRttiFromInterface(Action)));
    Result := Dispatcher(Action);
  end;
end;

end.

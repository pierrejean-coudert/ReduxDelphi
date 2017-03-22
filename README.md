# ReduxDelphi

ReduxDelphi is a predictable state container for Delphi apps utilizing a unidirectional data flow. Inspired by https://github.com/reactjs/redux . 

This project is currently developped with Delphi 10.1

The TodoMVC sample is based on immutable data structures (Generic immutable lists).


## Counter sample code

Here is a simple example showing the basic usage of Store, Actions, Dispatch,...:

```Pascal
unit DemoCounterForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Redux;

type
  TEnumAction= (INIT, INCREMENT, DECREMENT);

  TFormDemoCounter = class(TForm)
    ButtonInc: TButton;
    ButtonDec: TButton;
    LabelCounter: TLabel;

    procedure ButtonIncClick(Sender: TObject);
    procedure ButtonDecClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    FStore : IStore<Integer, TEnumAction>;
  end;

var
  FormDemoCounter: TFormDemoCounter;

implementation

{$R *.dfm}

procedure TFormDemoCounter.ButtonIncClick(Sender: TObject);
begin
  FStore.dispatch(INCREMENT);
end;

procedure TFormDemoCounter.ButtonDecClick(Sender: TObject);
begin
  FStore.dispatch(DECREMENT);
end;

procedure TFormDemoCounter.FormShow(Sender: TObject);
var
  FReducer : TReducer<Integer,TEnumAction>;
begin
  FReducer :=
    function(State: Integer; Action: TEnumAction): Integer
    begin
      case Action of
        INCREMENT:
          Result := State + 1;
        DECREMENT:
          Result := State - 1;
        else
          Result := State;
      end;
    end;

  FStore := TStore<Integer, TEnumAction>.Create(FReducer, 0);
  FStore.subscribe( procedure (State: Integer)
    begin
      LabelCounter.Caption := IntToStr(State);
    end
  );
  FStore.dispatch(INIT);
end;

end.
```

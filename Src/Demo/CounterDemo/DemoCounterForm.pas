unit DemoCounterForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Redux;

type
  TAction= (INIT, INCREMENT, DECREMENT);

  TFormDemoCounter = class(TForm)
    ButtonInc: TButton;
    ButtonDec: TButton;
    Label1: TLabel;
    Memo1: TMemo;

    procedure ButtonIncClick(Sender: TObject);
    procedure ButtonDecClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    FStore : TStore<Integer, TAction>;
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
  FReducer : TReducer<Integer,TAction>;
begin
  FReducer :=
    function(State: Integer; Action: TAction): Integer
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

  FStore := TStore<Integer, TAction>.Create(FReducer, 0);

	FStore.subscribe( procedure (State: Integer)
    begin
      Label1.Caption := IntToStr(State);
    end
  );

	FStore.subscribe( procedure (State: Integer)
    begin
      Memo1.Lines.Add(IntToStr(State));
    end
  );

  FStore.dispatch(INIT);
end;

end.

unit TextDemoForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Redux, StdAction;

type
  TState = record
    MyText1: string;
    MyText2: string;
  end;

  TActionSetText1 = class(TActionSetText);
  TActionSetText2 = class(TActionSetText);


  TForm1 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    LabelSaved: TLabel;
    ButtonSave: TButton;
    procedure Edit1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);

  private
    FStore : TStore<TState, IAction>;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  InitState : TState = (MyText1:'1';
                        MyText2:'2');

{ TForm1 }

procedure TForm1.ButtonSaveClick(Sender: TObject);
begin
  FStore.Dispatch(TActionSetText2.Create(Edit1.Text));
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  FStore.Dispatch(TActionSetText1.Create(Edit1.Text));
end;

procedure TForm1.FormShow(Sender: TObject);
var
  FReducer : TReducer<TState, IAction>;
begin
  FReducer :=
    function(State: TState; Action: IAction): TState
    begin
      Result := State;
      if Action is TActionSetText1 then
        Result.MyText1 := TActionSetText1(Action).Text;
      if Action is TActionSetText2 then
        Result.MyText2 := TActionSetText2(Action).Text;
    end;

  FStore := TStore<TState, IAction>.Create(FReducer, InitState);

	FStore.subscribe( procedure (State: TState)
    begin
      Label1.Caption := State.MyText1;
      LabelSaved.Caption := State.MyText2;
    end
  );

  FStore.Dispatch(TActionInit.Create());
end;

end.

unit StdAction;

interface

type
  IAction = interface
  end;

  TActionInit = class(TInterfacedObject, IAction)
  end;

  TActionSetText = class abstract(TInterfacedObject, IAction)
    Text : string;
    constructor Create(AText: String);
  end;


implementation

{ TActionSetText }

constructor TActionSetText.Create(AText: String);
begin
  Text := AText;
end;

end.

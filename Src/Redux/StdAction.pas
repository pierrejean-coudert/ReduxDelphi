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

  TActionSetBool= class abstract(TInterfacedObject, IAction)
    Value : Boolean;
    constructor Create(AValue: Boolean);
  end;

  TActionSetGUID= class abstract(TInterfacedObject, IAction)
    GUID : TGUID;
    constructor Create(AGUID: TGUID);
  end;


implementation

{ TActionSetText }

constructor TActionSetText.Create(AText: String);
begin
  Text := AText;
end;

{ TActionSetBool }

constructor TActionSetBool.Create(AValue: Boolean);
begin
  Value := AValue;
end;

{ TActionSetGUID }

constructor TActionSetGUID.Create(AGUID: TGUID);
begin
  GUID := AGUID;
end;

end.

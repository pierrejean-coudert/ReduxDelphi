unit Immutable;

interface

uses
   System.Generics.Collections;

type
  TFilter<TItem> = reference to function(AItem: TItem): Boolean;
  TMapper<TItem> = reference to function(AItem: TItem): TItem;

  IImmutableList<TItem> = interface
    function Insert(Index: Integer; AItem: TItem): IImmutableList<TItem> ;
    function Filter(AFilter: TFilter<TItem>): IImmutableList<TItem> ;
    function Map(AMapper: TMapper<TItem>): IImmutableList<TItem> ;

    function GetEnumerator: TEnumerator<TItem>;
  end;


  TImmutableList<TItem> = class(TInterfacedObject, IImmutableList<TItem>)
  private
    FList: TList<TItem>;
  public
    constructor Create(); overload;
    constructor Create(AImmutableList: IImmutableList<TItem>); overload;
    destructor  Destroy; override;

    function Insert(Index: Integer; AItem: TItem): IImmutableList<TItem>;
    function Filter(AFilter: TFilter<TItem>): IImmutableList<TItem>;
    function Map(AMapper: TMapper<TItem>): IImmutableList<TItem>;

    function GetEnumerator: TEnumerator<TItem>;
  end;

implementation

{ TImmutableList<TItem> }

constructor TImmutableList<TItem>.Create;
begin
  FList := TList<TItem>.Create;
end;

constructor TImmutableList<TItem>.Create(AImmutableList: IImmutableList<TItem>);
var
  AItem : TItem;
begin
  FList := TList<TItem>.Create;
  for AItem in AImmutableList do
    FList.Add(AItem);
end;

destructor TImmutableList<TItem>.Destroy;
begin
  FList.Free;
  inherited;
end;

function TImmutableList<TItem>.GetEnumerator: TEnumerator<TItem>;
begin
  Result := FList.GetEnumerator;
end;

function TImmutableList<TItem>.Insert(Index: Integer; AItem: TItem): IImmutableList<TItem>;
var
  NewList :  TImmutableList<TItem>;
begin
  NewList := TImmutableList<TItem>.Create(Self);
  TImmutableList<TItem>(NewList).FList.Insert(Index, AItem);
  Result := NewList;
end;

function TImmutableList<TItem>.Filter(AFilter: TFilter<TItem>): IImmutableList<TItem>;
var
  AItem : TItem;
  NewList : TImmutableList<TItem>;
begin
  NewList := TImmutableList<TItem>.Create();
  for AItem in FList do begin
    if AFilter(AItem) then
      TImmutableList<TItem>(NewList).FList.Add(AItem)
  end;
  Result := NewList;
end;

function TImmutableList<TItem>.Map(AMapper: TMapper<TItem>): IImmutableList<TItem>;
var
  AItem : TItem;
  NewList : TImmutableList<TItem>;
begin
  NewList := TImmutableList<TItem>.Create();
  for AItem in FList do begin
      TImmutableList<TItem>(NewList).FList.Add( AMapper(AItem))
  end;
  Result := NewList;
end;
end.

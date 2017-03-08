unit Immutable;

interface

uses
   System.Generics.Collections;

type
  TFilter<T> = reference to function(AItem: T): Boolean;
  TMapper<T> = reference to function(AItem: T): T;

  IImmutableList<T> = interface
    function Insert(Index: Integer; AItem: T): IImmutableList<T> ;
    function Filter(AFilter: TFilter<T>): IImmutableList<T> ;
    function Map(AMapper: TMapper<T>): IImmutableList<T> ;

    function Count: Integer;
    function Items(Index: Integer): T;

    function GetEnumerator: TEnumerator<T>;
  end;


  TImmutableList<T> = class(TInterfacedObject, IImmutableList<T>)
  private
    FList: TList<T>;
  public
    constructor Create(); overload;
    constructor Create(AImmutableList: IImmutableList<T>); overload;
    destructor  Destroy; override;

    function Insert(Index: Integer; AItem: T): IImmutableList<T>;
    function Filter(AFilter: TFilter<T>): IImmutableList<T>;
    function Map(AMapper: TMapper<T>): IImmutableList<T>;

    function Count: Integer;
    function Items(Index: Integer): T;

    function GetEnumerator: TEnumerator<T>;
  end;

implementation

{ TImmutableList<T> }

constructor TImmutableList<T>.Create;
begin
  FList := TList<T>.Create;
end;

function TImmutableList<T>.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TImmutableList<T>.Create(AImmutableList: IImmutableList<T>);
var
  AItem : T;
begin
  FList := TList<T>.Create;
  for AItem in AImmutableList do
    FList.Add(AItem);
end;

destructor TImmutableList<T>.Destroy;
begin
  FList.Free;
  inherited;
end;

function TImmutableList<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := FList.GetEnumerator;
end;

function TImmutableList<T>.Insert(Index: Integer; AItem: T): IImmutableList<T>;
var
  NewList :  TImmutableList<T>;
begin
  NewList := TImmutableList<T>.Create(Self);
  TImmutableList<T>(NewList).FList.Insert(Index, AItem);
  Result := NewList;
end;

function TImmutableList<T>.Items(Index: Integer): T;
begin
  Result := FList.Items[Index];
end;

function TImmutableList<T>.Filter(AFilter: TFilter<T>): IImmutableList<T>;
var
  AItem : T;
  NewList : TImmutableList<T>;
begin
  NewList := TImmutableList<T>.Create();
  for AItem in FList do begin
    if AFilter(AItem) then
      TImmutableList<T>(NewList).FList.Add(AItem)
  end;
  Result := NewList;
end;

function TImmutableList<T>.Map(AMapper: TMapper<T>): IImmutableList<T>;
var
  AItem : T;
  NewList : TImmutableList<T>;
begin
  NewList := TImmutableList<T>.Create();
  for AItem in FList do begin
      TImmutableList<T>(NewList).FList.Add( AMapper(AItem))
  end;
  Result := NewList;
end;
end.

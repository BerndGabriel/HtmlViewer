{
Version   12
Copyright (c) 2011 by Bernd Gabriel

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Note that the source modules HTMLGIF1.PAS and DITHERUNIT.PAS
are covered by separate copyright notices located in those modules.
}

{$I htmlcons.inc}

unit BegaClasses;

interface

uses
  Classes,
  HtmlGlobals;

type
  TBegaCustomMap = class
  private
    FKeys: TList;
    FValues: TList;
    FSorted: Boolean;
    procedure SetSorted(const Value: Boolean);
  protected
    function GetCount: Integer; {$ifdef UseInline} inline; {$endif}
    function GetKey(Index: Integer): Pointer; {$ifdef UseInline} inline; {$endif}
    function GetValue(Index: Integer): Pointer; {$ifdef UseInline} inline; {$endif}
    procedure CreateLists; virtual;
    property Keys[Index: Integer]: Pointer read GetKey;
    property Values[Index: Integer]: Pointer read GetValue;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    //
    function Find(Key: Pointer; out Index: Integer): Boolean;
    function Get(Key: Pointer): Pointer; {$ifdef UseInline} inline; {$endif}
    function Put(Key, Value: Pointer): Pointer; {$ifdef UseInline} inline; {$endif}
    function Remove(Key: Pointer): Pointer; {$ifdef UseInline} inline; {$endif}
    procedure Assign(Source: TBegaCustomMap); {$ifdef UseInline} inline; {$endif}
    procedure Clear; {$ifdef UseInline} inline; {$endif}
    property Count: Integer read GetCount;
    property Sorted: Boolean read FSorted write SetSorted;
  end;

  TBegaMap = class(TBegaCustomMap)
  public
    property Keys;
    property Values; default;
  end;
implementation

{ TBegaCustomMap }

//-- BG ---------------------------------------------------------- 18.04.2011 --
procedure TBegaCustomMap.AfterConstruction;
begin
  inherited;
  CreateLists;
end;

procedure TBegaCustomMap.Assign(Source: TBegaCustomMap);
var
  I: Integer;
begin
  Clear;
  FSorted := Source.FSorted;
  if Source <> nil then
    for I := 0 to Source.Count - 1 do
      Put(Source.Keys[I], Source.Values[I]);
end;

//-- BG ---------------------------------------------------------- 18.04.2011 --
procedure TBegaCustomMap.BeforeDestruction;
begin
  FKeys.Free;
  FValues.Free;
  inherited;
end;

procedure TBegaCustomMap.Clear;
begin
  FKeys.Clear;
  FValues.Clear;
end;

//-- BG ---------------------------------------------------------- 18.04.2011 --
procedure TBegaCustomMap.CreateLists;
begin
  FKeys := TList.Create;
  FValues := TList.Create;
end;

//-- BG ---------------------------------------------------------- 18.04.2011 --
function TBegaCustomMap.Find(Key: Pointer; out Index: Integer): Boolean;
var
  Low, Mid, High: Integer;
begin
  if Sorted then
  begin
    // simple binary search to find (first) occurence of Key:
    Low := 0;
    High := Count;
    while Low < High do
    begin
      Mid := (Low + High) shr 1;
      if PtrUInt(FKeys[Mid]) < PtrUInt(Key) then
        Low := Mid + 1
      else
        High := Mid;
    end;
    Index := High;
    Result := (Index < Count) and (PtrUInt(FKeys[Index]) = PtrUInt(Key));
  end
  else
  begin
    Index := FKeys.IndexOf(Key);
    Result := Index >= 0;
    if not Result then
      Index := FKeys.Count;
  end;
end;

//-- BG ---------------------------------------------------------- 18.04.2011 --
function TBegaCustomMap.Get(Key: Pointer): Pointer;
var
  Index: Integer;
begin
  if Find(Key, Index) then
    Result := FValues[Index]
  else
    Result := nil;
end;

//-- BG ---------------------------------------------------------- 18.04.2011 --
function TBegaCustomMap.GetCount: Integer;
begin
  Result := FKeys.Count;
end;

//-- BG ---------------------------------------------------------- 18.04.2011 --
function TBegaCustomMap.GetKey(Index: Integer): Pointer;
begin
  Result := FKeys[Index];
end;

//-- BG ---------------------------------------------------------- 18.04.2011 --
function TBegaCustomMap.GetValue(Index: Integer): Pointer;
begin
  Result := FValues[Index];
end;

//-- BG ---------------------------------------------------------- 18.04.2011 --
function TBegaCustomMap.Put(Key, Value: Pointer): Pointer;
var
  Index: Integer;
begin
  if Find(Key, Index) then
  begin
    Result := FValues[Index];
    FValues[Index] := Value;
  end
  else
  begin
    Result := nil;
    FKeys.Insert(Index, Key);
    FValues.Insert(Index, Value);
  end;
end;

//-- BG ---------------------------------------------------------- 18.04.2011 --
function TBegaCustomMap.Remove(Key: Pointer): Pointer;
var
  Index: Integer;
begin
  if Find(Key, Index) then
  begin
    Result := FValues[Index];
    FKeys.Delete(Index);
    FValues.Delete(Index);
  end
  else
    Result := nil;
end;

//-- BG ---------------------------------------------------------- 18.04.2011 --
procedure TBegaCustomMap.SetSorted(const Value: Boolean);
var
  SortedMap: TBegaCustomMap;
begin
  if FSorted <> Value then
  begin
    if Value and (Count > 1) then
    begin
      SortedMap := TBegaCustomMap.Create;
      try
        SortedMap.Sorted := True;
        SortedMap.Assign(Self);
        Assign(SortedMap);
      finally
        SortedMap.Free;
      end;
    end;
    FSorted := Value;
  end;
end;

end.

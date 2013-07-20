{
Version   12
Copyright (c) 1995-2008 by L. David Baldwin,
Copyright (c) 2008-2010 by HtmlViewer Team
Copyright (c) 2011-2012 by Bernd Gabriel

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

unit HtmlCaches;

interface

uses
  SysUtils,
  HtmlGlobals;

type

  //BG, 30.04.2011:
  ThtCachable = class(TObject)
  private
    AccessCount: Integer;
    UsageCount: Integer; {how many times in use}
  public
    destructor Destroy; override;
    procedure BeginUse;
    procedure EndUse;
  end;


//------------------------------------------------------------------------------
// ThtCache is the cache, that holds the above cacheable items.
//------------------------------------------------------------------------------

  ThtCache = class(ThtStringList)
  private
    FMaxCache: Integer;
    function GetObject(Index: Integer): ThtCachable; reintroduce;
  protected
    function GetCachable(I: Integer): ThtCachable; {$ifdef UseInline} inline; {$endif}
    property Objects[Index: Integer]: ThtCachable read GetObject;
  public
    constructor Create;
    destructor Destroy; override;
    function AddObject(const S: ThtString; AObject: ThtCachable): Integer; reintroduce;
    procedure BumpAndCheck;
    procedure Clear; override;
    procedure DecUsage(const S: ThtString);
    procedure IncUsage(const S: ThtString);
    procedure PurgeCache;
    procedure SetCacheCount(N: Integer);
    property MaxCache: Integer read FMaxCache;
  end;

implementation

{ ThtCachable }

//-- BG ---------------------------------------------------------- 16.04.2011 --
procedure ThtCachable.BeginUse;
begin
  Inc(UsageCount);
end;

//-- BG ---------------------------------------------------------- 16.04.2011 --
destructor ThtCachable.Destroy;
begin
  Assert(UsageCount = 0, 'Freeing cached ' + ClassName + ' that''s still in use ' + IntToStr(UsageCount) + ' time/s.' );
  inherited;
end;

//-- BG ---------------------------------------------------------- 16.04.2011 --
procedure ThtCachable.EndUse;
begin
  Dec(UsageCount);
  Assert(UsageCount >= 0, 'Cached ' + ClassName + ' usage count < 0');
end;

{ ThtCache }

//------------------------------------------------------------------------------
function ThtCache.AddObject(const S: ThtString; AObject: ThtCachable): Integer;
begin
  Result := inherited AddObject(S, AObject);
  Inc(AObject.UsageCount);
end;

//------------------------------------------------------------------------------
procedure ThtCache.BumpAndCheck;
var
  I: Integer;
  Tmp: ThtCachable;
begin
  for I := Count - 1 downto 0 do
  begin
    Tmp := Objects[I];
    Inc(Tmp.AccessCount);
    if (Tmp.AccessCount > FMaxCache) and (Tmp.UsageCount <= 0) then
    begin
      Delete(I);
      Tmp.Free; {the ThtCachable}
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure ThtCache.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Objects[I].Free;
  inherited Clear;
end;

//------------------------------------------------------------------------------
constructor ThtCache.Create;
begin
  inherited Create;
  FMaxCache := 100;
  Sorted := True;
end;

//------------------------------------------------------------------------------
procedure ThtCache.DecUsage(const S: ThtString);
var
  I: Integer;
begin
  I := IndexOf(S);
  if I >= 0 then
    Objects[I].EndUse;
end;

//------------------------------------------------------------------------------
destructor ThtCache.Destroy;
begin
  Clear;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
function ThtCache.GetCachable(I: Integer): ThtCachable;
begin
  Result := Objects[I];
  if Result <> nil then
  begin
    Result.AccessCount := 0;
    Inc(Result.UsageCount);
  end;
end;

//-- BG ---------------------------------------------------------- 06.03.2011 --
function ThtCache.GetObject(Index: Integer): ThtCachable;
begin
  Result := ThtCachable(inherited GetObject(Index));
end;

//------------------------------------------------------------------------------
procedure ThtCache.IncUsage(const S: ThtString);
var
  I: Integer;
begin
  I := IndexOf(S);
  if I >= 0 then
    Objects[I].BeginUse;
end;

//------------------------------------------------------------------------------
procedure ThtCache.PurgeCache;
var
  I: Integer;
  Tmp: ThtCachable;
begin
  for I := Count - 1 downto 0 do
  begin
    Tmp := Objects[I];
    if (Tmp.UsageCount <= 0) then
    begin
      Delete(I);
      Tmp.Free; {the ThtCachable}
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure ThtCache.SetCacheCount(N: Integer);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    with Objects[I] do
    begin
      if (AccessCount > N) and (UsageCount <= 0) then
      begin
        Delete(I);
        Free;
      end;
    end;
  FMaxCache := N;
end;

end.

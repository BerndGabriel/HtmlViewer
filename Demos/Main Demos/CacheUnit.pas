{
Version   11.7
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2016 by HtmlViewer Team

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
{$ifdef ver140}
{$Warn Symbol_Platform Off}
{$endif}
{$ifdef ver150}  {Delphi 7}
{$Warn Symbol_Platform Off}
{$endif}

unit CacheUnit;

{$include htmlcons.inc}

interface

uses
  {$ifdef LCL}
    LCLIntf, LCLType, LMessages,
  {$else}
    WinTypes, WinProcs,
  {$endif}
  Messages, SysUtils, Classes, Controls, StdCtrls, Math, URLSubs;

const
  MaxCacheEntries = 200;
  CacheData = 'cachinfo.txt';
var
  Seq: Integer = 10000;

type
  TDiskCache = class(TObject)
  private
    URLList, RedirectList, TypeList, FileList: TStringList;
    Directory: string;
    procedure CheckSize;
    function MakeName(S: string): string;
    function SplitProtocol(var URL: string): string;
    procedure Erase(I: Integer);
    function IsCacheable(const Protocol: string): Boolean;
  public
    constructor Create(const ADirectory: string);
    destructor Destroy; override;
    function AddNameToCache(const URL, NewURL: string; DocType: ThtDocType; Error: boolean): string;
      {Given an URL, creates a filename and adds both to cachelist}
    function GetCacheFilename(const URL: string; var Filename: string; var DocType: ThtDocType; var NewURL: string): boolean;
    procedure RemoveFromCache(const URL: string);
    procedure EraseCache;
    procedure Flush;
  end;

implementation

{$ifndef Delphi7_Plus}
uses
  FileCtrl;
{$endif}

constructor TDiskCache.Create(const ADirectory: string);
var
  F: TextFile;
  I: integer;
  S, S1, S2: string;
begin
  inherited Create;
  Directory := ADirectory;
  if not DirectoryExists(Directory) then
    ForceDirectories(Directory);
  URLList := TStringList.Create;
  TypeList := TStringList.Create;
  FileList := TStringList.Create;
  RedirectList := TStringList.Create;
  if FileExists(Directory+CacheData) then
  begin
    AssignFile(F, Directory+CacheData);
    Reset(F);
    while not EOF(F) do
    begin
      ReadLn(F, S);
      I := Pos(' ', S);
      if I >= 0 then
      begin
        S1 := Trim(Copy(S, 1, I-1));
        S := Copy(S, I+1, Length(S)-I);
        I := Pos(' ', S);
        S2 := Trim(Copy(S, 1, I-1));
        S := Copy(S, I+1, Length(S)-I);
        I := Pos(' ', S);
        if I >= 0 then
        begin
          URLList.Add(S1);
          RedirectList.Add(S2);
          TypeList.Add(Trim(Copy(S, 1, I-1)));
          FileList.Add(Trim(Copy(S, I+1, Length(S)-I)));
        end;
      end;
    end;
    with FileList do
      if Count > 0 then
        Seq := StrToIntDef(Copy(Strings[Count-1], 2, 5), 10000)+1;
    CloseFile(F);
  end;
end;

destructor TDiskCache.Destroy;
begin
  URLList.Free;
  TypeList.Free;
  FileList.Free;
  RedirectList.Free;
  inherited Destroy;
end;

//-- BG ---------------------------------------------------------- 20.05.2016 --
// extracted from Destroy
procedure TDiskCache.Flush;
var
  F: TextFile;
  I: Integer;
begin
  AssignFile(F, Directory+CacheData);
  Rewrite(F);
  for I := 0 to URLList.Count-1 do
    WriteLn(F, URLList[I], ' ', RedirectList[I], ' ', TypeList[I], ' ', FileList[I]);
  CloseFile(F);
end;

function TDiskCache.IsCacheable(const Protocol: string): Boolean;
begin
{$ifdef UseSSL}
  Result := Pos('http', Protocol) > 0;  {cache only http's and https's}
{$else}
  Result := Protocol = 'http';
{$endif}
end;

function TDiskCache.AddNameToCache(const URL, NewURL: string; DocType: ThtDocType; Error: boolean): string;
var
  I: Integer;
  S, P: string;
begin
  S := URL;
  P := SplitProtocol(S);
  SetLength(Result, 0);
  if IsCacheable(P) then
  begin
    I := URLList.IndexOf(S);
    if I >= 0 then
      Result := FileList[I]
    else
    begin
      Result := MakeName(S);
      if Error then
        S := S + '*';   {so it won't be found in a lookup}
      TypeList.Add(IntToStr(Ord(DocType)));
      URLList.Add(S);
      FileList.Add(Result);
      if NewURL <> '' then
        RedirectList.Add(NewURL)
      else
        RedirectList.Add('*');
      CheckSize;
    end;
    Result := Directory + Result;
  end;
end;

function TDiskCache.GetCacheFilename(const URL: string; var Filename: string;
         var DocType: ThtDocType; var NewURL: string): boolean;
var
  I: Integer;
  S, P: string;
begin
  NewURL := '';
  S := URL;
  P := SplitProtocol(S);
  Result := IsCacheable(P);
  if Result then
  begin
    I := URLList.IndexOf(S);
    DocType := HTMLType;
    Result := I >= 0;
    if Result then
    begin
      Filename := Directory+FileList[I];
      DocType := ThtDocType(StrToIntDef(TypeList[I], 0));
      S := RedirectList[I];
      if S <> '*' then
        NewURL := S
      else
        NewURL := '';
    end
    else
    begin  {try searching in the Redirect list.  The back button would generate this}
      I := RedirectList.IndexOf(Lowercase(URL));
      Result := I >= 0;
      if Result then
      begin
        Filename := Directory+FileList[I];
        DocType := ThtDocType(StrToIntDef(TypeList[I], 0));
        NewUrl := '';
      end
      else
        Filename := '';
    end;
  end;
end;

procedure TDiskCache.CheckSize;
begin
  while FileList.Count > MaxCacheEntries do
  begin
    DeleteFile(Directory+FileList[0]);
    FileList.Delete(0);
    URLlist.Delete(0);
    TypeList.Delete(0);
    RedirectList.Delete(0);
  end;
end;

function TDiskCache.MakeName(S: string): string;
{S is lower case here}
var
  I: Integer;
  Ext: string;
  GoodExt: Boolean;
begin
  Ext := '';
  for I := Length(S) downto Max(1, Length(S)-5) do
    if S[I] = '.' then
    begin
      Ext := Copy(S, I, 255);
      Break;
    end;
  GoodExt := (Ext <>'') and
           ((Ext = '.htm') or (Ext = '.html') or (Ext = '.xhtml') or (Ext = '.xht') or
            (Ext = '.gif') or (Ext = '.jpg') or
            (Ext = '.png') or (Ext = '.bmp') or (Ext = '.txt') or (Ext = '.jpeg') or
            (Ext = '.css'));
  Result := 'm'+IntToStr(Seq);
  Inc(Seq);
  if GoodExt then
    Result := Result+Ext;
end;

procedure TDiskCache.Erase(I: Integer);
begin
  DeleteFile(Directory+FileList[I]);
  FileList.Delete(I);
  URLList.Delete(I);
  TypeList.Delete(I);
  RedirectList.Delete(I);
end;

procedure TDiskCache.RemoveFromCache(const URL: string);
var
  I: Integer;
  S: string;
begin
  S := URL;
  SplitProtocol(S);
  I := URLList.IndexOf(S);
  if I >= 0 then
    Erase(I);
end;

procedure TDiskCache.EraseCache;
var
  I: Integer;
begin
  for I := FileList.Count-1 downto 0 do
    Erase(I);
end;

function TDiskCache.SplitProtocol(var URL: string): string;
var
  I: Integer;
begin
  I := 1;
  if FindSchemeSep(Url, I) then
  begin
    Result := LowerCase(Copy(Url, 1, I - 1));
    Delete(URL, 1, I + 2);
  end
  else
    // default protocol
    Result := 'http';

  if CompareText('www.', Copy(URL, 1, 4)) = 0 then
    Delete(URL, 1, 4);
end;

end.

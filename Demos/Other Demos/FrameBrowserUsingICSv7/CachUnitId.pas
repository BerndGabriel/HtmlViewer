{$ifdef ver140}
{$Warn Symbol_Platform Off}
{$endif}
{$ifdef ver150}  {Delphi 7}
{$Warn Symbol_Platform Off}
{$endif}

unit CachUnitId;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Controls, StdCtrls, Math,
  htmlview;

const
  MaxCacheEntries = 200;   
  CacheData = 'cachinfo.txt';
var
  Seq: integer = 12345;

type
  TDiskCache = class(TObject)
  private
    URLList, RedirectList, TypeList, FileList: TStringList;   
    Directory: string;
    procedure CheckSize;
    function MakeName(S: string): string;
    function RemovePrefix(const URL: string): string;
  public
    constructor Create(const ADirectory: string);
    destructor Destroy; override;
    function AddNameToCache(const URL, NewURL: string; DocType: ThtmlFileType; Error: boolean): string;
      {Given an URL, creates a filename and adds both to cachelist}
    function GetCacheFilename(const URL: string; var Filename: string;
             var DocType: ThtmlFileType; var NewURL: string): boolean;
    procedure RemoveFromCache(URL: string);
    procedure EraseCache;
  end;

implementation

uses
  FBUnitIcs, htmlun2, urlsubs;

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
      Seq := StrToIntDef(Copy(Strings[Count-1], 2, 5), 1110)+1;
  CloseFile(F);
  end;
end;

destructor TDiskCache.Destroy;
var
  F: TextFile;
  I: integer;
begin
if Monitor1 then  {only write if first instance}
  begin
  AssignFile(F, Directory+CacheData);
  Rewrite(F);
  for I := 0 to URLList.Count-1 do
    WriteLn(F, URLList[I], ' ', RedirectList[I], ' ', TypeList[I], ' ', FileList[I]);  
  CloseFile(F);
  end;
URLList.Free;
TypeList.Free;
FileList.Free;
RedirectList.Free;
inherited Destroy;
end;

function TDiskCache.AddNameToCache(const URL, NewURL: string; DocType: ThtmlFileType; Error: boolean): string;
var
  I: integer;
  S: string;
begin
{$ifdef UseSSL}
if Pos('http', GetProtocol(URL)) = 0 then  {cache only http's and https's}
{$else}
S := GetProtocol(URL);
if S <> 'http' then
{$endif}
  begin
  Result := '';
  Exit;
  end;
S := RemovePrefix(URL);
I := URLList.IndexOf(S);
if I >= 0 then
  Result := FileList[I]
else
  begin
  Result := MakeName(S);
  if Error then S := S+'*';   {so it won't be found in a lookup}
  TypeList.Add(IntToStr(Ord(DocType)));
  URLList.Add(S);
  FileList.Add(Result);
  if NewURL <> '' then    
    RedirectList.Add(NewURL)
  else RedirectList.Add('*');
  CheckSize;
  end;
Result := Directory+Result;
end;

function TDiskCache.GetCacheFilename(const URL: string; var Filename: string;
         var DocType: ThtmlFileType; var NewURL: string): boolean;
var
  I: integer;
  S: string;
begin
NewURL := '';
S := RemovePrefix(URL);
I := URLList.IndexOf(S);
DocType := HTMLType;
Result := I >= 0;
if Result then
  begin
  Filename := Directory+FileList[I];
  DocType := ThtmlFileType(StrToIntDef(TypeList[I], 0));
  S := RedirectList[I];   
  if S <> '*' then
   NewURL := S
  else NewURL := '';
  end
else
  begin  {try searching in the Redirect list.  The back button would generate this}  
  I := RedirectList.IndexOf(Lowercase(URL));
  Result := I >= 0;
  if Result then
    begin
    Filename := Directory+FileList[I];
    DocType := ThtmlFileType(StrToIntDef(TypeList[I], 0));
    NewUrl := '';
    end
  else Filename := '';
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
  Ext: string;
  GoodExt: boolean;
begin
Ext := ExtractFileExt(S);
GoodExt := (Ext <>'') and
           ((Ext = '.htm') or  (Ext = '.html') or (Ext = '.gif') or (Ext = '.jpg') or
            (Ext = '.png') or  (Ext = '.bmp') or (Ext = '.txt') or (Ext = '.jpeg') or
            (Ext = '.css'));  
Result := 'm'+IntToStr(Seq);
Inc(Seq);
if GoodExt then
  Result := Result+Ext;
end;

procedure TDiskCache.RemoveFromCache(URL: string);
var
  I: integer;
  S: string;
begin
S := RemovePrefix(URL);
I := URLList.IndexOf(S);
if I >=0 then
  begin
  DeleteFile(Directory+FileList[I]);   
  FileList.Delete(I);
  TypeList.Delete(I);
  URLlist.Delete(I);
  Redirectlist.Delete(I); 
  end;
end;

procedure TDiskCache.EraseCache;
var
  I: integer;
begin
for I := FileList.Count-1 downto 0 do
  begin
  DeleteFile(Directory+FileList[I]);
  FileList.Delete(I);
  URLList.Delete(I);
  TypeList.Delete(I);
  RedirectList.Delete(I);  
  end;
end;

function TDiskCache.RemovePrefix(const URL: string): string;
var
  I: integer;
begin
Result := Lowercase(URL);
I := Pos('https://', Result);
if I = 0 then
  I := Pos('http://', Result);   
if I > 0 then
  Result := Copy(Result, I+7, Length(Result));
I := Pos('www.', Result);
if I = 1 then
  Result := Copy(Result, I+4, Length(Result));
end;

end.

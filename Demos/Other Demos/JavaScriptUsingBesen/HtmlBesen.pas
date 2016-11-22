{
Version   11.7
Copyright (c) 2016 by B.Gabriel

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

unit HtmlBesen;

{$include ..\..\..\source\htmlcons.inc}

interface

uses
{$ifdef LCL}
//  LclIntf, LclType, Types, //LclProc, HtmlMisc,
{$else}
  Windows,
{$endif}
  Classes,
  Controls,
  SysUtils,
  Forms,
  Dialogs,

  HtmlGlobals,
  HtmlUn2,
  HtmlView,
  LogFormUnit,

  BESEN,
  BESENConstants,
  BESENErrors,
  BESENNumberUtils,
  BESENObject,
  BESENObjectPropertyDescriptor,
  BESENStringUtils,
  BESENTypes,
  BESENValue,
  BESENVersionConstants;

type
  ThtScriptEngine = class(TBESEN)
  private
    FForm: TForm;
    FLogForm: TLogForm;
    procedure Init;
    procedure NativeAlert(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
    procedure NativeConfirm(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
    procedure NativePrompt(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
  public
    constructor Create(Form: TForm; LogForm: TLogForm = nil);
    procedure DoOnScript(Sender: TObject; const Name, ContentType, Src, Script: ThtString);
    function VersionNo: ThtString;
  end;


implementation

{ ThtScriptEngine }

//-- BG ---------------------------------------------------------- 13.11.2016 --
constructor ThtScriptEngine.Create(Form: TForm; LogForm: TLogForm);
begin
  inherited Create(COMPAT_JS);
  FForm := Form;
  FLogForm := LogForm;
  Init;
end;

//-- BG ---------------------------------------------------------- 13.11.2016 --
procedure ThtScriptEngine.Init;
var
  ObjDocument, ObjNavigator, ObjWindow: TBESENObject;
begin
  ObjWindow := ObjectGlobal;
  ObjWindow.OverwriteData('window', BESENObjectValue(ObjWindow), [bopaWRITABLE,bopaCONFIGURABLE]);
  ObjWindow.RegisterNativeFunction('alert'  , NativeAlert   , 1, []);
  ObjWindow.RegisterNativeFunction('confirm', NativeConfirm , 1, []);
  ObjWindow.RegisterNativeFunction('prompt' , NativePrompt  , 2, []);

  ObjDocument := TBESENObject.Create(Self, ObjectPrototype, false);
  GarbageCollector.Add(ObjDocument);
  ObjWindow.OverwriteData('document', BESENObjectValue(ObjDocument), [bopaWRITABLE,bopaCONFIGURABLE]);

  ObjNavigator := TBESENObject.Create(Self, ObjectPrototype, false);
  GarbageCollector.Add(ObjNavigator);
  ObjWindow.OverwriteData('navigator', BESENObjectValue(ObjNavigator), [bopaWRITABLE,bopaCONFIGURABLE]);
  ObjNavigator.OverwriteData('userAgent', BESENStringValue('HtmlViewer, Version (V) ' + HtmlUn2.VersionNo + ', Copyright (C) 2016 by B.Gabriel'), [bopaWRITABLE,bopaCONFIGURABLE]);
end;

//-- BG ---------------------------------------------------------- 13.11.2016 --
procedure ThtScriptEngine.DoOnScript(Sender: TObject; const Name, ContentType, Src, Script: ThtString);
var
  MyMessage: string;
  MySrc, MySrcPathDummy: ThtString;
  OldCursor: TCursor;
  HtmlViewer: THtmlViewer absolute Sender;
  Stream: TStream;
  MyScript: TBESENANSISTRING;
begin
  MyScript := UTF8Encode(Script);
  if Length(Script) > 0 then
    MySrc := Name
  else if Length(Src) > 0 then
  begin
    MySrc := Src;
    Stream := nil;
    HtmlViewer.htStreamRequest(MySrc, Stream, MySrcPathDummy);
    try
      if (Stream <> nil) and (Stream.Size > 0) then
      begin
        Stream.Position := 0;
        SetLength(MyScript, Stream.Size);
        Stream.Read(MyScript[1], Stream.Size);
      end;
    finally
      HtmlViewer.htStreamRequested(MySrc, Stream);
    end;
  end;

  if Length(MyScript) > 0 then
  begin
    if (FLogForm <> nil) and FLogForm.LogActive[laJavaScript] then
      FLogForm.Log('Executing ' + ContentType + ' "' + MySrc + '" ...');
    try
      OldCursor := Screen.Cursor;
      Screen.Cursor := crHourGlass;
      try
        Execute(BESENConvertToUTF8(MyScript));
      finally
        Screen.Cursor := OldCursor;
      end;
      MyMessage := 'Done.';
    except
      on E: EBESENError do
        MyMessage := E.Name + ': ' + E.Message;
      on E: Exception do
        MyMessage := 'Error: ' + E.Message;
    end;
    if (FLogForm <> nil) and FLogForm.LogActive[laJavaScript] then
      FLogForm.Log(MyMessage);
  end;
end;

//-- BG ---------------------------------------------------------- 11.11.2016 --
procedure ThtScriptEngine.NativeAlert(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
var
  Message: ThtString;
begin
  if CountArguments > 0 then
    Message := ToStr(Arguments^[0]^);

{$ifdef LCL}
  MessageDlg(FForm.Caption, Message, mtInformation, [mbOk], 0);
{$else}
  MessageBoxW(FForm.Handle, PWideChar(Message), PWideChar(FForm.Caption), MB_ICONINFORMATION + MB_OK);
{$endif}
  ResultValue.ValueType := bvtUNDEFINED;
end;

//-- BG ---------------------------------------------------------- 11.11.2016 --
procedure ThtScriptEngine.NativeConfirm(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
var
  Message: ThtString;
  Result: Integer;
begin
  if CountArguments > 0 then
    Message := ToStr(Arguments^[0]^);

{$ifdef LCL}
  Result := MessageDlg(FForm.Caption, Message, mtConfirmation, [mbYes, mbNo], 0);
{$else}
  Result := MessageBoxW(FForm.Handle, PWideChar(Message), PWideChar(FForm.Caption), MB_ICONQUESTION + MB_YESNO);
{$endif}
  ResultValue := BESENBooleanValue(Result = mrYes);
end;

//-- BG ---------------------------------------------------------- 11.11.2016 --
procedure ThtScriptEngine.NativePrompt(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
var
  Message, Result: ThtString;
begin
  if CountArguments > 0 then
    Message := ToStr(Arguments^[0]^);

  if CountArguments > 1 then
    Result := ToStr(Arguments^[1]^);

  Result := InputBox(FForm.Caption, Message, Result);
  ResultValue := BESENStringValue(Result);
end;

//-- BG ---------------------------------------------------------- 13.11.2016 --
function ThtScriptEngine.VersionNo: ThtString;
begin
  Result := BESENVersion;
end;

end.

object LogForm: TLogForm
  Left = 0
  Top = 0
  Caption = 'Diagnostic Window'
  ClientHeight = 410
  ClientWidth = 641
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LogMemo: TMemo
    Left = 0
    Top = 0
    Width = 641
    Height = 410
    Align = alClient
    PopupMenu = PopupMenu
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
    OnDblClick = LogMemoDblClick
    OnKeyDown = LogMemoKeyDown
  end
  object PopupMenu: TPopupMenu
    Left = 310
    Top = 205
    object pCopy: TMenuItem
      Caption = 'Copy Ctl+C'
      OnClick = pCopyClick
    end
    object pCopyAll: TMenuItem
      Caption = 'Copy All'
      OnClick = pCopyAllClick
    end
    object pClear: TMenuItem
      Caption = 'Clear'
      OnClick = pClearClick
    end
  end
end

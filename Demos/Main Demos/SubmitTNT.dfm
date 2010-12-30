object SubmitFormTnt: TSubmitFormTnt
  Left = 1020
  Top = 291
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Submit Results'
  ClientHeight = 273
  ClientWidth = 427
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 16
    Top = 19
    Width = 41
    Height = 16
    Caption = 'Action:'
  end
  object Label2: TLabel
    Left = 9
    Top = 43
    Width = 47
    Height = 16
    Caption = 'Method:'
  end
  object Label3: TLabel
    Left = 18
    Top = 71
    Width = 44
    Height = 16
    Caption = 'Results'
  end
  object Button1: TButton
    Left = 172
    Top = 242
    Width = 73
    Height = 28
    Caption = '&Close'
    Default = True
    TabOrder = 3
    OnClick = Button1Click
  end
  object ActionText: TTntEdit
    Left = 68
    Top = 14
    Width = 342
    Height = 24
    TabStop = False
    ReadOnly = True
    TabOrder = 0
  end
  object MethodText: TTntEdit
    Left = 68
    Top = 41
    Width = 342
    Height = 24
    TabStop = False
    ReadOnly = True
    TabOrder = 1
  end
  object ResultBox: TTntListBox
    Left = 17
    Top = 90
    Width = 392
    Height = 145
    TabStop = False
    ItemHeight = 16
    TabOrder = 2
  end
end

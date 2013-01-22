object ProxyForm: TProxyForm
  Left = 298
  Top = 180
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Proxy'
  ClientHeight = 207
  ClientWidth = 288
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 26
    Height = 13
    Caption = 'Proxy'
  end
  object Label2: TLabel
    Left = 216
    Top = 16
    Width = 19
    Height = 13
    Caption = 'Port'
  end
  object Label3: TLabel
    Left = 24
    Top = 64
    Width = 77
    Height = 13
    Caption = 'Proxy Username'
  end
  object Label4: TLabel
    Left = 24
    Top = 112
    Width = 75
    Height = 13
    Caption = 'Proxy Password'
  end
  object ProxyEdit: TEdit
    Left = 24
    Top = 32
    Width = 177
    Height = 21
    TabOrder = 0
  end
  object PortEdit: TEdit
    Left = 216
    Top = 32
    Width = 49
    Height = 21
    TabOrder = 1
    Text = '80'
  end
  object OKBurron: TBitBtn
    Left = 64
    Top = 168
    Width = 65
    Height = 25
    TabOrder = 2
    Kind = bkOK
  end
  object CancelButton: TBitBtn
    Left = 160
    Top = 168
    Width = 65
    Height = 25
    TabOrder = 3
    Kind = bkCancel
  end
  object ProxyUsername: TEdit
    Left = 24
    Top = 80
    Width = 177
    Height = 21
    TabOrder = 4
  end
  object ProxyPassword: TEdit
    Left = 24
    Top = 128
    Width = 177
    Height = 21
    TabOrder = 5
  end
end

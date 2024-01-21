unit Unit1;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, SnapMB,
  ZConnection;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    M: TMemo;
    ZConnection1: TZConnection;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    Dev: TSnapMBBroker;
    Registers: Array[0..9] of WORD;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses ModbusTransfer;

{ TForm1 }

procedure CheckError(Res: Integer);
begin
  if res <> 0 then
    raise Exception.Create('Error: $' + IntToHex(Res, 8));
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  res: Integer;
begin
  dev := TSnapMBBroker.Create(sfRTU, 'COM1', 9600, 'E', 8, 1, flowNone);
  //dev := TSnapMBBroker.Create;
  //res := dev.AddDevice(sfRTU, 1, 'COM1', 9600, 'E', 8, 1, flowNone);
  //CheckError(res);
  Dev.ChangeTo(sfRTU, 'COM1', 9600, 'N', 8, 1, flowNone);
  Dev.SetLocalParam(par_BaseAddressZero, 1);
  Dev.SetLocalParam(par_MaxRetries, 2);
  res := Dev.Connect;
  CheckError(res);
  Res := Dev.ReadHoldingRegisters(1, 1, 10, @Registers[0]);
  CheckError(res);
  ShowMessage(IntToStr(Registers[2]));
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  res: Integer;
begin
  Res := Dev.ReadHoldingRegisters(1, 1, 10, @Registers[0]);
  CheckError(res);
  ShowMessage(IntToStr(Registers[2]));
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Action1: TMbtModbusAction;
begin
  Action1 := TMbtModbusAction.Create;
  Action1.BaudRate := 9600;
  Action1.ComPort := 'COM1';
  Action1.DataBits := 8;
  Action1.DeviceAddress := 1;
  Action1.FlowControl := flowNone;
  Action1.ModbusFormat := sfRTU;
  Action1.Parity := 'N';
  Action1.;
end;

end.


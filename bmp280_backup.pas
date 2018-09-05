unit bmp280_backup;

{$mode delphi}
{$H+}
{$inline on}   {Allow use of Inline procedures}

interface

uses
  Classes,
  SysUtils,
  math,
  GlobalConst,
  GlobalConfig,
  DAQ_Globals,
  Devices,
  BCM2710,
  ADS8320,
  SPI,
  I2C;

const
  BMP280_ADRESS_CALIB   = $88 ;

  BMP280_ADRESS_T1     = $88 - BMP280_ADRESS_CALIB + $01;
  BMP280_ADRESS_T2     = $8A - BMP280_ADRESS_CALIB + $01 ;
  BMP280_ADRESS_T3     = $8C - BMP280_ADRESS_CALIB + $01 ;

  BMP280_ADRESS_P1     = $8E - BMP280_ADRESS_CALIB + $01 ;
  BMP280_ADRESS_P2     = $90 - BMP280_ADRESS_CALIB + $01 ;
  BMP280_ADRESS_P3     = $92 - BMP280_ADRESS_CALIB + $01 ;
  BMP280_ADRESS_P4     = $94 - BMP280_ADRESS_CALIB + $01 ;
  BMP280_ADRESS_P5     = $96 - BMP280_ADRESS_CALIB + $01 ;
  BMP280_ADRESS_P6     = $98 - BMP280_ADRESS_CALIB + $01 ;
  BMP280_ADRESS_P7     = $9A - BMP280_ADRESS_CALIB + $01 ;
  BMP280_ADRESS_P8     = $9C - BMP280_ADRESS_CALIB + $01 ;
  BMP280_ADRESS_P9     = $9E - BMP280_ADRESS_CALIB + $01 ;

  BMP280_ADRESS_CHIP_ID   = $D0 ;
  BMP280_ADRESS_RESET     = $E0 ;
  BMP280_ADRESS_STATUS    = $F3 ;
  BMP280_ADRESS_CTRL_MEAS = $F4 ;
  BMP280_ADRESS_CONFIG    = $F5 ;

  BMP280_ADRESS_RAWDATA = $F7 ;

  BMP280_ADRESS_P_MSB   = $F7 - BMP280_ADRESS_RAWDATA + $01 ;
  BMP280_ADRESS_P_LSB   = $F8 - BMP280_ADRESS_RAWDATA + $01 ;
  BMP280_ADRESS_P_XSB   = $F9 - BMP280_ADRESS_RAWDATA + $01 ;
  BMP280_ADRESS_T_MSB   = $FA - BMP280_ADRESS_RAWDATA + $01 ;
  BMP280_ADRESS_T_LSB   = $FB - BMP280_ADRESS_RAWDATA + $01 ;
  BMP280_ADRESS_T_XSB   = $FC - BMP280_ADRESS_RAWDATA + $01 ;

  BMP280_OS_MS_0   = 0 ;
  BMP280_OS_MS_1   = 1 ;
  BMP280_OS_MS_2   = 2 ;
  BMP280_OS_MS_3   = 4 ;
  BMP280_OS_MS_4   = 8 ;
  BMP280_OS_MS_5   = 16;

  BMP280_I2C_ADRESS = $76;

{type

  TBMP_CalibBytes = array[0..24] of Byte;
  PBMP280_CalibData =  ^TBMP_CalibBytes;

  TBPM_ReadBytes = array[0..6] of Byte;
  PBMP280_ReadData =  ^TBPM_ReadBytes;

  TBPM_WriteBytes = array[0..1] of Byte;
  PBMP280_WriteData =  ^TBPM_WriteBytes;}

//var


procedure BMP280_startDevice(var BMPDevice:PI2CDevice;var mode:Byte; out n_Bytes:LongWord);
procedure BMP280_write_register(BMPDevice:Pointer;BMPWriteData:PBMP280_WriteData; out n_Bytes:LongWord);
procedure BMP280_read_register(BMPDevice:Pointer;RegAdress:Byte;BMPReadData:PBMP280_ReadData);
procedure BMP280_LoadCalibration(BMPDevice:Pointer;BMPCalibData:PBMP280_CalibData);
procedure BMP280_read_raw_data(BMPDevice:Pointer;out raw_t,raw_p:LongWord; var bytes_Count:LongWord);
procedure BMP280_read_data(BMPDevice:Pointer; out p,T:Double; out raw_t,raw_p : LongWord; var bytes_Count:LongWord);
procedure BMP280_read_register2(BMPDevice:Pointer;RegAdress:PBMP280_RegByte;BMPRecData:PBMP280_ReceiveBytes;n_Bytes:Byte;var bytes_Count:LongWord);
procedure BMP280_write_register2(BMPDevice:Pointer;RegAdress:PBMP280_RegByte;BMPWriteData:PBMP280_WriteData; out n_Bytes:LongWord);

function u16(Data:array of Byte;Pos:Byte):Word;
function s16(Data:array of Byte;Pos:Byte):Int16;
function u8(Data:array of Byte;Pos:Byte):Byte;
function s8(Data:array of Byte;Pos:Byte):Int8;
implementation

procedure BMP280_startDevice(var BMPDevice:PI2CDevice;var mode:Byte; out n_Bytes:LongWord);
var
  i,BMP_Config_Byte:Byte;
  I2C_Return:LongWord;
  n_Bytes2:LongWord;

  p,T:Double;
  raw_t,raw_p : LongWord;
begin
  BMP_MODE:=0;
  if mode=1 then
    begin
      BMP_MODE   := 1;
      BMPDevice  := I2CDeviceFindByDescription(BCM2710_I2C1_DESCRIPTION);
      I2C_Return := I2CDeviceStart(BMPDevice, 0);
      if I2C_Return <> ERROR_SUCCESS then
        begin
          mode:=I2C_Return;
          Exit;

        end;
    end;

  // Configure BMP:
  BMP_Config_Byte:=1;
  BMPReg[0]:=BMP280_ADRESS_CONFIG;
  //BMPWriteData[0]:=BMP280_ADRESS_CONFIG;
  BMPWriteData[0]:=BMP_Config_Byte;
  BMP280_write_register2(BMPDevice,BMPReg,BMPWriteData,n_Bytes);
  BMP280_read_data(BMPDevice,p,T,raw_t,raw_p,n_Bytes);
  Sleep(100);
  BMP280_read_data(BMPDevice,p,T,raw_t,raw_p,n_Bytes);
  //BMP280_read_register(BMPDevice,BMP280_ADRESS_CONFIG,BMPReadData);
  // Load Calibration Data:
  //BMP280_LoadCalibration(BMPDevice,BMPCalibData);
  BMPReg[0]:= BMP280_ADRESS_CALIB ;
  BMP280_read_register2(BMPDevice,BMPReg,BMPRecData,24,n_Bytes2);
  for i:=1 to (SizeOf(TBMP_CalibBytes)-1) do BMPCalibData[i]:=BMPRecData[i-1];

  BMP_CALIB_T1 := u16(BMPCalibData^,BMP280_ADRESS_T1);
  BMP_CALIB_T2 := s16(BMPCalibData^,BMP280_ADRESS_T2);
  BMP_CALIB_T3 := s16(BMPCalibData^,BMP280_ADRESS_T3);

  BMP_CALIB_P1 := u16(BMPCalibData^,BMP280_ADRESS_P1);
  BMP_CALIB_P2 := s16(BMPCalibData^,BMP280_ADRESS_P2);
  BMP_CALIB_P3 := s16(BMPCalibData^,BMP280_ADRESS_P3);
  BMP_CALIB_P4 := s16(BMPCalibData^,BMP280_ADRESS_P4);
  BMP_CALIB_P5 := s16(BMPCalibData^,BMP280_ADRESS_P5);
  BMP_CALIB_P6 := s16(BMPCalibData^,BMP280_ADRESS_P6);
  BMP_CALIB_P7 := s16(BMPCalibData^,BMP280_ADRESS_P7);
  BMP_CALIB_P8 := s16(BMPCalibData^,BMP280_ADRESS_P8);
  BMP_CALIB_P9 := s16(BMPCalibData^,BMP280_ADRESS_P9);

  BMP_MEAS_TIME:=(1.25+2.3*1)+(0.575+2.3*1);
  //Result:=BMPDevice;
end;

procedure BMP280_write_register(BMPDevice:Pointer;BMPWriteData:PBMP280_WriteData; out n_Bytes:LongWord);
var
   DataLength:Word;
   i : Word;
   //n_Bytes:LongWord;
   arraySize:Word;
begin
  n_Bytes:=0;
  arraySize:=SizeOf(TBPM_WriteBytes);
  if BMP_MODE=1 then
    begin
      I2CDeviceWrite(BMPDevice,BMP280_I2C_ADRESS,BMPWriteData,arraySize,  n_Bytes);
    end
  else
    begin
      DataLength := Floor((arraySize-1)/2);
      for i:=0 to DataLength do BMPWriteData[i*2]:= BMPWriteData[i*2] and $7F;

      //SPIDeviceWriteRead(BMPDevice,SPI_CS_1,BMPWriteData,nil,arraySize,SPI_TRANSFER_NONE,n_Bytes);
      ADS8320SPI0WriteRead(BMPDevice ,SPI_CS_1, BMPWriteData,nil,arraySize, SPI_TRANSFER_NONE, n_Bytes);
    end;
end;

procedure BMP280_write_register2(BMPDevice:Pointer;RegAdress:PBMP280_RegByte;BMPWriteData:PBMP280_WriteData; out n_Bytes:LongWord);
var
   DataLength:Word;
   i : Word;
   //n_Bytes:LongWord;
   arraySize:Word;
begin
  n_Bytes:=0;
  arraySize:=SizeOf(TBPM_WriteBytes);
  if BMP_MODE=1 then
    begin
      I2CDeviceWriteWrite(BMPDevice,BMP280_I2C_ADRESS,RegAdress,1,BMPWriteData,1,  n_Bytes);
    end
  else
    begin
      DataLength := Floor((arraySize-1)/2);
      for i:=0 to DataLength do BMPWriteData[i*2]:= BMPWriteData[i*2] and $7F;

      //SPIDeviceWriteRead(BMPDevice,SPI_CS_1,BMPWriteData,nil,arraySize,SPI_TRANSFER_NONE,n_Bytes);
      ADS8320SPI0WriteRead(BMPDevice ,SPI_CS_1, BMPWriteData,nil,arraySize, SPI_TRANSFER_NONE, n_Bytes);
    end;
end;

procedure BMP280_read_register(BMPDevice:Pointer;RegAdress:Byte;BMPReadData:PBMP280_ReadData);
var
   i : Word;
   n_Bytes:LongWord;
   arraySize:Word;
   //I2CReadPointer:^array[0..5] of Byte;
begin
  n_Bytes:=0;
  arraySize:=SizeOf(TBPM_ReadBytes);

  for i:=0 to arraySize-1 do BMPReadData^[i]:=0;
  BMPReadData^[0]:=RegAdress;

  if BMP_MODE=1 then
    begin
      //I2CDeviceWrite(BMPDevice,BMP280_I2C_ADRESS,BMPWriteData,arraySize,  n_Bytes);
      I2CDeviceWriteRead(BMPDevice,BMP280_I2C_ADRESS,BMPReadData,1,BMPReadData,arraySize-1,n_Bytes);
      for i:=arraySize-1 downto 1 do
        begin
          BMPReadData^[i]:=BMPReadData^[i-1]
        end;
    end
  else
    begin
      BMPReadData[0]:=BMPReadData[0] or $80;
      //SPIDeviceWriteRead(BMPDevice,SPI_CS_1,BMPReadData,BMPReadData,arraySize,SPI_TRANSFER_NONE,n_Bytes);
      ADS8320SPI0WriteRead(BMPDevice ,SPI_CS_1,BMPReadData,BMPReadData,arraySize, SPI_TRANSFER_NONE, n_Bytes);
    end;
end;

procedure BMP280_read_register2(BMPDevice:Pointer;RegAdress:PBMP280_RegByte;BMPRecData:PBMP280_ReceiveBytes;n_Bytes:Byte;var bytes_Count:LongWord);
var
   i : Word;
   //bytes_Count:LongWord;
   arraySize:Word;
begin


  bytes_Count:=0;
  for i:=0 to SizeOf(TBMP_ReceiveBytes)-1 do BMPRecData^[i]:=10;

  if BMP_MODE=1 then
    begin
      //I2CDeviceWrite(BMPDevice,BMP280_I2C_ADRESS,BMPWriteData,arraySize,  n_Bytes);
      I2CDeviceWriteRead(BMPDevice,BMP280_I2C_ADRESS,RegAdress,1,BMPRecData,n_Bytes,bytes_Count);
      {for i:=n_Bytes downto 1 do
        begin
          BMPCalibData^[i]:=BMPCalibData^[i-1]
        end;}
    end
  else
    begin
      BMPCalibData[0]:=BMPCalibData[0] or $80;
      //ointerWriteRead(BMPDevice,SPI_CS_1,BMPCalibData,BMPCalibData,arraySize,SPI_TRANSFER_NONE,n_Bytes);
      ADS8320SPI0WriteRead(BMPDevice ,SPI_CS_1,BMPCalibData,BMPCalibData,n_Bytes, SPI_TRANSFER_NONE, bytes_Count);
    end;

  //BMP280_read_register(BMPDevice,BMP280_ADRESS_CALIB,BMPCalibData)
end;

procedure BMP280_LoadCalibration(BMPDevice:Pointer;BMPCalibData:PBMP280_CalibData);
var
   i : Word;
   n_Bytes:LongWord;
   arraySize:Word;
begin

  n_Bytes:=0;
  arraySize:=SizeOf(TBMP_CalibBytes);

  for i:=0 to arraySize-1 do BMPCalibData^[i]:=0;
  BMPCalibData^[0]:=BMP280_ADRESS_CALIB;

  if BMP_MODE=1 then
    begin
      //I2CDeviceWrite(BMPDevice,BMP280_I2C_ADRESS,BMPWriteData,arraySize,  n_Bytes);
      I2CDeviceWriteRead(BMPDevice,BMP280_I2C_ADRESS,BMPCalibData,1,BMPCalibData,arraySize-1,n_Bytes);
      for i:=arraySize-1 downto 1 do
        begin
          BMPCalibData^[i]:=BMPCalibData^[i-1]
        end;
    end
  else
    begin
      BMPCalibData[0]:=BMPCalibData[0] or $80;
      //ointerWriteRead(BMPDevice,SPI_CS_1,BMPCalibData,BMPCalibData,arraySize,SPI_TRANSFER_NONE,n_Bytes);
      ADS8320SPI0WriteRead(BMPDevice ,SPI_CS_1,BMPCalibData,BMPCalibData,arraySize, SPI_TRANSFER_NONE, n_Bytes);
    end;

  //BMP280_read_register(BMPDevice,BMP280_ADRESS_CALIB,BMPCalibData)
end;

procedure BMP280_read_raw_data(BMPDevice:Pointer;out raw_t,raw_p:LongWord; var bytes_Count:LongWord);
var
   i,msb,lsb,xlsb:Byte;
begin



  //BMP280_read_register(BMPDevice,BMP280_ADRESS_RAWDATA,BMPReadData);
  BMPReg[0]:=BMP280_ADRESS_RAWDATA ;
  BMP280_read_register2(BMPDevice,BMPReg,BMPRecData,6,bytes_Count);
  for i:=1 to (SizeOf(TBPM_ReadBytes)-1) do BMPReadData[i]:=BMPRecData[i-1];

  msb:=u8(BMPReadData^,BMP280_ADRESS_P_MSB);
  lsb:=u8(BMPReadData^,BMP280_ADRESS_P_LSB);
  xlsb:=u8(BMPReadData^,BMP280_ADRESS_P_XSB);

  raw_p:=((msb shl 16) or (lsb shl 8) or xlsb) shr 4 ;

  msb:=u8(BMPReadData^,BMP280_ADRESS_T_MSB);
  lsb:=u8(BMPReadData^,BMP280_ADRESS_T_LSB);
  xlsb:=u8(BMPReadData^,BMP280_ADRESS_T_XSB);

  raw_t:=((msb shl 16) or (lsb shl 8) or xlsb) shr 4 ;

end;

procedure BMP280_init_forced_meas(BMPDevice:Pointer; out n_Bytes:longWord);
var
   i:Byte;
   n_Bytes2:LongWord;
begin
  n_Bytes:=0;
  BMPReg[0]:=BMP280_ADRESS_CTRL_MEAS;
  BMPWriteData[0]:=BMP280_ADRESS_CTRL_MEAS;
  BMPWriteData[1]:=(1 shl 5) or (1 shl 2) or 1;
  BMP280_write_register(BMPDevice,BMPWriteData,n_Bytes);

  BMP280_read_register2(BMPDevice,BMPReg,BMPRecData,2,n_Bytes2);
  for i:=1 to (SizeOf(TBPM_WriteBytes)-1) do BMPWriteData[i]:=BMPRecData[i-1];

  Sleep(ceil(BMP_MEAS_TIME));
  Sleep(1000);
end;

procedure BMP280_read_data(BMPDevice:Pointer; out p,T:Double; out raw_t,raw_p : LongWord; var bytes_Count:LongWord);
var

   var1,var2,T_fine : Double;
   bytes_Count2:LongWord;
begin

   BMP280_init_forced_meas(BMPDevice,bytes_Count);

   BMP280_read_raw_data(BMPDevice,raw_t,raw_p,bytes_Count2);

   var1 := (raw_t/16384.0 - BMP_CALIB_T1/1024.0) * BMP_CALIB_T2;
   var2 := ((raw_t/131072.0 - BMP_CALIB_T1/8192.0) * (raw_t/131072.0 - BMP_CALIB_T1/8192.0)) * BMP_CALIB_T3;
   T_fine:=var1+var2;
   T:=(var1+var2)/5120.0;

   var1:=T_fine/2.0 - 64000.0;
   var2 := var1 * var1 * BMP_CALIB_P6 / 32768.0;
   var2 := var2 + (var1 * BMP_CALIB_P5 * 2.0);
   var2 := (var2/4.0)+(BMP_CALIB_P4 * 65536.0);
   var1 := ((BMP_CALIB_P3 * var1 * var1 / 524288.0) + (BMP_CALIB_P2 * var1)) / 524288.0;
   var1 := (1.0 + var1 / 32768.0)*BMP_CALIB_P1;
   if var1 <> 0.0 then
     begin
         p := 1048576.0 - raw_p;
         p := (p - (var2 / 4096.0)) * 6250.0 / var1;
         var1 := BMP_CALIB_P9 * p * p / 2147483648.0;
         var2 := p * BMP_CALIB_P8 / 32768.0;
         p := p + (var1 + var2 + BMP_CALIB_P7) / 16.0;
     end
   else p := 0;
end;

function u16(Data:array of Byte;Pos:Byte):Word;
begin
  Result:=(Data[Pos] or (Data[Pos+1] shl 8))
end;
function u8(Data:array of Byte;Pos:Byte):Byte;
begin
  Result:=Data[Pos]
end;

function s16(Data:array of Byte;Pos:Byte):Int16;
var
   u16_in:Word;
begin
  u16_in:=u16(Data,Pos);
  if u16_in > 32767 then Result:=u16_in-65536
  else Result:=u16_in;
end;
function s8(Data:array of Byte;Pos:Byte):Int8;
var
   u8_in:Byte;
begin
  u8_in:=u8(Data,Pos);
  if u8_in > 127 then Result:=u8_in-256
  else Result:=u8_in;
end;

end.


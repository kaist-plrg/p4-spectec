(* Copyright 2018-present Cornell University
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
 *)

open Interface.Unpack

[@@@ocamlformat "disable"]

(* CRC16-ARC table *)
let crc16_table = [|
  0x0000; 0xC0C1; 0xC181; 0x0140; 0xC301; 0x03C0; 0x0280; 0xC241;
  0xC601; 0x06C0; 0x0780; 0xC741; 0x0500; 0xC5C1; 0xC481; 0x0440;
  0xCC01; 0x0CC0; 0x0D80; 0xCD41; 0x0F00; 0xCFC1; 0xCE81; 0x0E40;
  0x0A00; 0xCAC1; 0xCB81; 0x0B40; 0xC901; 0x09C0; 0x0880; 0xC841;
  0xD801; 0x18C0; 0x1980; 0xD941; 0x1B00; 0xDBC1; 0xDA81; 0x1A40;
  0x1E00; 0xDEC1; 0xDF81; 0x1F40; 0xDD01; 0x1DC0; 0x1C80; 0xDC41;
  0x1400; 0xD4C1; 0xD581; 0x1540; 0xD701; 0x17C0; 0x1680; 0xD641;
  0xD201; 0x12C0; 0x1380; 0xD341; 0x1100; 0xD1C1; 0xD081; 0x1040;
  0xF001; 0x30C0; 0x3180; 0xF141; 0x3300; 0xF3C1; 0xF281; 0x3240;
  0x3600; 0xF6C1; 0xF781; 0x3740; 0xF501; 0x35C0; 0x3480; 0xF441;
  0x3C00; 0xFCC1; 0xFD81; 0x3D40; 0xFF01; 0x3FC0; 0x3E80; 0xFE41;
  0xFA01; 0x3AC0; 0x3B80; 0xFB41; 0x3900; 0xF9C1; 0xF881; 0x3840;
  0x2800; 0xE8C1; 0xE981; 0x2940; 0xEB01; 0x2BC0; 0x2A80; 0xEA41;
  0xEE01; 0x2EC0; 0x2F80; 0xEF41; 0x2D00; 0xEDC1; 0xEC81; 0x2C40;
  0xE401; 0x24C0; 0x2580; 0xE541; 0x2700; 0xE7C1; 0xE681; 0x2640;
  0x2200; 0xE2C1; 0xE381; 0x2340; 0xE101; 0x21C0; 0x2080; 0xE041;
  0xA001; 0x60C0; 0x6180; 0xA141; 0x6300; 0xA3C1; 0xA281; 0x6240;
  0x6600; 0xA6C1; 0xA781; 0x6740; 0xA501; 0x65C0; 0x6480; 0xA441;
  0x6C00; 0xACC1; 0xAD81; 0x6D40; 0xAF01; 0x6FC0; 0x6E80; 0xAE41;
  0xAA01; 0x6AC0; 0x6B80; 0xAB41; 0x6900; 0xA9C1; 0xA881; 0x6840;
  0x7800; 0xB8C1; 0xB981; 0x7940; 0xBB01; 0x7BC0; 0x7A80; 0xBA41;
  0xBE01; 0x7EC0; 0x7F80; 0xBF41; 0x7D00; 0xBDC1; 0xBC81; 0x7C40;
  0xB401; 0x74C0; 0x7580; 0xB541; 0x7700; 0xB7C1; 0xB681; 0x7640;
  0x7200; 0xB2C1; 0xB381; 0x7340; 0xB101; 0x71C0; 0x7080; 0xB041;
  0x5000; 0x90C1; 0x9181; 0x5140; 0x9301; 0x53C0; 0x5280; 0x9241;
  0x9601; 0x56C0; 0x5780; 0x9741; 0x5500; 0x95C1; 0x9481; 0x5440;
  0x9C01; 0x5CC0; 0x5D80; 0x9D41; 0x5F00; 0x9FC1; 0x9E81; 0x5E40;
  0x5A00; 0x9AC1; 0x9B81; 0x5B40; 0x9901; 0x59C0; 0x5880; 0x9841;
  0x8801; 0x48C0; 0x4980; 0x8941; 0x4B00; 0x8BC1; 0x8A81; 0x4A40;
  0x4E00; 0x8EC1; 0x8F81; 0x4F40; 0x8D01; 0x4DC0; 0x4C80; 0x8C41;
  0x4400; 0x84C1; 0x8581; 0x4540; 0x8701; 0x47C0; 0x4680; 0x8641;
  0x8201; 0x42C0; 0x4380; 0x8341; 0x4100; 0x81C1; 0x8081; 0x4040;
|]

(* CRC32 table *)
let crc32_table = [|
  0X00000000; 0X77073096; 0XEE0E612C; 0X990951BA; 0X076DC419; 0X706AF48F; 0XE963A535; 0X9E6495A3;
  0X0EDB8832; 0X79DCB8A4; 0XE0D5E91E; 0X97D2D988; 0X09B64C2B; 0X7EB17CBD; 0XE7B82D07; 0X90BF1D91;
  0X1DB71064; 0X6AB020F2; 0XF3B97148; 0X84BE41DE; 0X1ADAD47D; 0X6DDDE4EB; 0XF4D4B551; 0X83D385C7;
  0X136C9856; 0X646BA8C0; 0XFD62F97A; 0X8A65C9EC; 0X14015C4F; 0X63066CD9; 0XFA0F3D63; 0X8D080DF5;
  0X3B6E20C8; 0X4C69105E; 0XD56041E4; 0XA2677172; 0X3C03E4D1; 0X4B04D447; 0XD20D85FD; 0XA50AB56B;
  0X35B5A8FA; 0X42B2986C; 0XDBBBC9D6; 0XACBCF940; 0X32D86CE3; 0X45DF5C75; 0XDCD60DCF; 0XABD13D59;
  0X26D930AC; 0X51DE003A; 0XC8D75180; 0XBFD06116; 0X21B4F4B5; 0X56B3C423; 0XCFBA9599; 0XB8BDA50F;
  0X2802B89E; 0X5F058808; 0XC60CD9B2; 0XB10BE924; 0X2F6F7C87; 0X58684C11; 0XC1611DAB; 0XB6662D3D;
  0X76DC4190; 0X01DB7106; 0X98D220BC; 0XEFD5102A; 0X71B18589; 0X06B6B51F; 0X9FBFE4A5; 0XE8B8D433;
  0X7807C9A2; 0X0F00F934; 0X9609A88E; 0XE10E9818; 0X7F6A0DBB; 0X086D3D2D; 0X91646C97; 0XE6635C01;
  0X6B6B51F4; 0X1C6C6162; 0X856530D8; 0XF262004E; 0X6C0695ED; 0X1B01A57B; 0X8208F4C1; 0XF50FC457;
  0X65B0D9C6; 0X12B7E950; 0X8BBEB8EA; 0XFCB9887C; 0X62DD1DDF; 0X15DA2D49; 0X8CD37CF3; 0XFBD44C65;
  0X4DB26158; 0X3AB551CE; 0XA3BC0074; 0XD4BB30E2; 0X4ADFA541; 0X3DD895D7; 0XA4D1C46D; 0XD3D6F4FB;
  0X4369E96A; 0X346ED9FC; 0XAD678846; 0XDA60B8D0; 0X44042D73; 0X33031DE5; 0XAA0A4C5F; 0XDD0D7CC9;
  0X5005713C; 0X270241AA; 0XBE0B1010; 0XC90C2086; 0X5768B525; 0X206F85B3; 0XB966D409; 0XCE61E49F;
  0X5EDEF90E; 0X29D9C998; 0XB0D09822; 0XC7D7A8B4; 0X59B33D17; 0X2EB40D81; 0XB7BD5C3B; 0XC0BA6CAD;
  0XEDB88320; 0X9ABFB3B6; 0X03B6E20C; 0X74B1D29A; 0XEAD54739; 0X9DD277AF; 0X04DB2615; 0X73DC1683;
  0XE3630B12; 0X94643B84; 0X0D6D6A3E; 0X7A6A5AA8; 0XE40ECF0B; 0X9309FF9D; 0X0A00AE27; 0X7D079EB1;
  0XF00F9344; 0X8708A3D2; 0X1E01F268; 0X6906C2FE; 0XF762575D; 0X806567CB; 0X196C3671; 0X6E6B06E7;
  0XFED41B76; 0X89D32BE0; 0X10DA7A5A; 0X67DD4ACC; 0XF9B9DF6F; 0X8EBEEFF9; 0X17B7BE43; 0X60B08ED5;
  0XD6D6A3E8; 0XA1D1937E; 0X38D8C2C4; 0X4FDFF252; 0XD1BB67F1; 0XA6BC5767; 0X3FB506DD; 0X48B2364B;
  0XD80D2BDA; 0XAF0A1B4C; 0X36034AF6; 0X41047A60; 0XDF60EFC3; 0XA867DF55; 0X316E8EEF; 0X4669BE79;
  0XCB61B38C; 0XBC66831A; 0X256FD2A0; 0X5268E236; 0XCC0C7795; 0XBB0B4703; 0X220216B9; 0X5505262F;
  0XC5BA3BBE; 0XB2BD0B28; 0X2BB45A92; 0X5CB36A04; 0XC2D7FFA7; 0XB5D0CF31; 0X2CD99E8B; 0X5BDEAE1D;
  0X9B64C2B0; 0XEC63F226; 0X756AA39C; 0X026D930A; 0X9C0906A9; 0XEB0E363F; 0X72076785; 0X05005713;
  0X95BF4A82; 0XE2B87A14; 0X7BB12BAE; 0X0CB61B38; 0X92D28E9B; 0XE5D5BE0D; 0X7CDCEFB7; 0X0BDBDF21;
  0X86D3D2D4; 0XF1D4E242; 0X68DDB3F8; 0X1FDA836E; 0X81BE16CD; 0XF6B9265B; 0X6FB077E1; 0X18B74777;
  0X88085AE6; 0XFF0F6A70; 0X66063BCA; 0X11010B5C; 0X8F659EFF; 0XF862AE69; 0X616BFFD3; 0X166CCF45;
  0XA00AE278; 0XD70DD2EE; 0X4E048354; 0X3903B3C2; 0XA7672661; 0XD06016F7; 0X4969474D; 0X3E6E77DB;
  0XAED16A4A; 0XD9D65ADC; 0X40DF0B66; 0X37D83BF0; 0XA9BCAE53; 0XDEBB9EC5; 0X47B2CF7F; 0X30B5FFE9;
  0XBDBDF21C; 0XCABAC28A; 0X53B39330; 0X24B4A3A6; 0XBAD03605; 0XCDD70693; 0X54DE5729; 0X23D967BF;
  0XB3667A2E; 0XC4614AB8; 0X5D681B02; 0X2A6F2B94; 0XB40BBE37; 0XC30C8EA1; 0X5A05DF1B; 0X2D02EF8D;
|]
[@@@ocamlformat "enable"]

(* Bitstring manipulation *)

let rec shift_bitstring_left (v : Bigint.t) (o : Bigint.t) : Bigint.t =
  if Bigint.(o > zero) then
    shift_bitstring_left Bigint.(v * (one + one)) Bigint.(o - one)
  else v

let rec shift_bitstring_right (v : Bigint.t) (o : Bigint.t) : Bigint.t =
  if Bigint.(o > zero) then
    let v_shifted = Bigint.(v / (one + one)) in
    shift_bitstring_right v_shifted Bigint.(o - one)
  else v

let power_of_two (w : Bigint.t) : Bigint.t = shift_bitstring_left Bigint.one w

let rec of_two_complement (n : Bigint.t) (w : Bigint.t) : Bigint.t =
  let w' = power_of_two w in
  if Bigint.(n >= w') then Bigint.(n % w')
  else if Bigint.(n < zero) then of_two_complement Bigint.(n + w') w
  else n

let slice_bitstring (n : Bigint.t) (m : Bigint.t) (l : Bigint.t) : Bigint.t =
  let slice_width = Bigint.(m + one - l) in
  if Bigint.(l < zero) then
    raise (Invalid_argument "bitslice x[y:z] must have y > z > 0");
  let shifted = Bigint.(n asr to_int_exn l) in
  let mask = Bigint.(power_of_two slice_width - one) in
  Bigint.bit_and shifted mask

let rec bitwise_neg (n : Bigint.t) (w : Bigint.t) : Bigint.t =
  if Bigint.(w > zero) then
    let w' = power_of_two Bigint.(w - one) in
    let g = slice_bitstring n Bigint.(w - one) Bigint.(w - one) in
    if Bigint.(g = zero) then bitwise_neg Bigint.(n + w') Bigint.(w - one)
    else bitwise_neg Bigint.(n - w') Bigint.(w - one)
  else n

(* Hash algorithms *)

let rec partition_bytes width value =
  if Bigint.(width = zero) then []
  else
    let hi = Bigint.(width - one) in
    let lo = Bigint.(width - of_int 8) in
    let byte_value = slice_bitstring value hi lo in
    let bytes_rest = slice_bitstring value Bigint.(lo - one) Bigint.zero in
    byte_value :: partition_bytes lo bytes_rest

let pad_right_to_16 ((width, value) : Bigint.t * Bigint.t) : Bigint.t * Bigint.t
    =
  let remainder : Bigint.t = Bigint.(width % of_int 16) in
  if Bigint.(zero = remainder) then (width, value)
  else
    let pad = Bigint.(of_int 16 - remainder) in
    (Bigint.(width + pad), value)

let compute_hash_crc16 (width : Bigint.t) (value : Bigint.t) : Bigint.t =
  let bytes_value = partition_bytes width value in
  List.fold_left
    (fun (hash_value : Bigint.t) (byte_value : Bigint.t) ->
      let hash_value_shifted =
        shift_bitstring_right hash_value (Bigint.of_int 8)
      in
      let crc_idx =
        Bigint.bit_xor hash_value byte_value
        |> Bigint.bit_and Bigint.(of_int 255)
        |> Bigint.to_int_exn
      in
      Bigint.bit_xor hash_value_shifted (crc16_table.(crc_idx) |> Bigint.of_int))
    Bigint.zero bytes_value

let compute_hash_crc32 (width : Bigint.t) (value : Bigint.t) : Bigint.t =
  let mask32 = Bigint.of_int 0xFFFFFFFF in
  let bytes_value = partition_bytes width value in
  List.fold_left
    (fun (hash_value : Bigint.t) (byte_value : Bigint.t) ->
      let hash_value_shifted =
        shift_bitstring_right hash_value (Bigint.of_int 8)
      in
      let crc_idx =
        Bigint.bit_xor hash_value byte_value
        |> Bigint.bit_and Bigint.(of_int 255)
        |> Bigint.to_int_exn
      in
      Bigint.bit_xor hash_value_shifted (crc32_table.(crc_idx) |> Bigint.of_int))
    mask32 bytes_value
  |> Bigint.bit_xor mask32

let compute_hash_csum16 (value_init : Bigint.t) (width : Bigint.t)
    (value : Bigint.t) : Bigint.t =
  let add_one_complement (v : Bigint.t) (w : Bigint.t) : Bigint.t =
    let tmp = Bigint.(v + w) in
    let thres = power_of_two (Bigint.of_int 16) in
    if Bigint.(tmp >= thres) then Bigint.((tmp % thres) + one)
    else Bigint.(tmp % thres)
  in
  let rec hash (value_hash : Bigint.t) (width : Bigint.t) (value : Bigint.t) =
    if Bigint.(width = zero) then value_hash
    else
      let msb = Bigint.(width - one) in
      let lsb = Bigint.(width - of_int 16) in
      let value_hash =
        add_one_complement value_hash (slice_bitstring value msb lsb)
      in
      let value = Bigint.(slice_bitstring value (msb - one) zero) in
      hash value_hash lsb value
  in
  let value_hash = hash value_init width value in
  bitwise_neg value_hash (Bigint.of_int 16)

let compute_hash_csum16_sub (value_init : Bigint.t) (width : Bigint.t)
    (value : Bigint.t) : Bigint.t =
  let add_one_complement (v : Bigint.t) (w : Bigint.t) : Bigint.t =
    let tmp = Bigint.(v + w) in
    let thres = power_of_two (Bigint.of_int 16) in
    if Bigint.(tmp >= thres) then Bigint.((tmp % thres) + one)
    else Bigint.(tmp % thres)
  in
  let rec hash (value_hash : Bigint.t) (width : Bigint.t) (value : Bigint.t) =
    if Bigint.(width = zero) then value_hash
    else
      let msb = Bigint.(width - one) in
      let lsb = Bigint.(width - of_int 16) in
      let value_hash =
        let value_neg =
          bitwise_neg (slice_bitstring value msb lsb) (Bigint.of_int 16)
        in
        add_one_complement value_hash value_neg
      in
      let value = Bigint.(slice_bitstring value (msb - one) zero) in
      hash value_hash lsb value
  in
  let value_hash = hash value_init width value in
  bitwise_neg value_hash (Bigint.of_int 16)

let adjust (base : Bigint.t) (rmax : Bigint.t) (value : Bigint.t) : Bigint.t =
  if Bigint.(rmax = zero) then base else Bigint.((value % (rmax - base)) + base)

(* Entry point *)

let compute_hash (algo : string) ?(value_init : Bigint.t = Bigint.zero)
    ((width, value) : Bigint.t * Bigint.t) : Bigint.t =
  match algo with
  | "csum16" -> compute_hash_csum16 value_init width value
  | "csum16_sub" -> compute_hash_csum16_sub value_init width value
  | "crc16" -> compute_hash_crc16 width value
  | "crc32" -> compute_hash_crc32 width value
  | "identity" -> value
  | _ -> Format.asprintf "(TODO: compute_hash) %s" algo |> failwith

let package (values : Value.t list) : Bigint.t * Bigint.t =
  values
  |> List.map unpack_p4_precision_numberValue
  |> List.map (fun (width, value) -> (width, of_two_complement value width))
  |> List.fold_left
       (fun (width_pack, value_pack) (width, value) ->
         let width_pack = Bigint.(width_pack + width) in
         let value_pack = shift_bitstring_left value_pack width in
         let value_pack = Bigint.(value_pack + value) in
         (width_pack, value_pack))
       (Bigint.zero, Bigint.zero)
  |> pad_right_to_16

let compute_checksum (algo : string) ?(value_init : Bigint.t = Bigint.zero)
    (values : Value.t list) : Bigint.t =
  values |> package |> compute_hash algo ~value_init

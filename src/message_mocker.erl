-module(message_mocker).
-export([start/8,main/0,pack_packet/3]).

main() ->
    start("127.0.0.1",9009,5,5,5000, 2, <<16#68f728063848:48>>, <<16#e8b1fc3306:40>>).

start(Host,Port,Parallel,Count,Delay, MUCount, ApMac, DevMacBase) ->
    io:format("%% --------------------------- %%~n"),
    io:format("Send to <~s:~B> ~n", [Host,Port]),
    Packet = pack_packet(MUCount, ApMac, DevMacBase),
    PacketSize = byte_size(Packet),
    io:format(" Count: ~B~n Size: ~B~n", [Parallel * Count * MUCount, Parallel * Count * PacketSize]),
    io:format("%% --------------------------- %%~n"),
    udp_broker:start(Host, Port, Parallel, Count, Delay, Packet),
    io:format("Done!",[]),
    ok.


pack_packet(MUCount,ApMac, DevMacBase) ->
    MagicId  = <<16#7c83>>,
    RequestId = <<0,0>>,
    AUcode = <<16#d8>>,
    AUSubCode = <<0>>,
    AL = 4 + 48 * MUCount,
    ALength = <<AL:16>>,
    Reserved = <<0,0>>,
    MUList = build_mu_list(ApMac, DevMacBase, <<0:0>>, MUCount),
    list_to_binary([MagicId,RequestId,AUcode,AUSubCode,ALength,MUCount,Reserved|MUList]).

    
build_mu_list(_, _, MUList, 0) -> MUList;
build_mu_list(APMac, DevMacBase, MUList, Count) ->
    MUCode = <<16#D6>>,
    MUSubCode = <<0,0>>,
    MULength = <<48:16>>,
    VendorId = <<16#A2E4>>,
    MUReserved = <<0,0>>,
    BSSID = <<0,0,0,0,0,0>>,
    MUReserved2 = <<0>>,
    RadioType = <<0>>,
    Channel = <<0>>,
    IsAssociated =  <<0>>,
    {Tms0,Tms1,_} = os:timestamp(),
    Tms = Tms0 * 1000000 + Tms1,
    Timestamp = <<Tms:32>>,
    MUReserved3 = <<0,0>>,
    MUType = <<0>>,
    MUReserved4 = <<0>>,
    Rr = random:uniform(20),
    RSSI = <<Rr:8>>,
    MUReserved5 = <<0>>,
    NoiseFloor = <<0>>,
    MUReserved6 = <<0,0,0>>,
    DataRate = <<0>>,
    MPDUFlags = <<0>>,
    Dl = random:uniform(127),
    MUMac = list_to_binary([DevMacBase,<<Dl:8>>]),
    FrameControl = <<16#ABCD:16>>,
    SequenceControl = <<0,0>>,
    MUReserved7 = <<0,0>>,
    MUListX = [MUCode,
	       MUSubCode,
	       MULength,
	       APMac,
	       VendorId,
	       MUReserved,
	       BSSID,
	       MUReserved2,
	       RadioType,
	       Channel,
	       IsAssociated,
	       Timestamp,
	       MUReserved3,
	       MUType,
	       MUReserved4,
	       RSSI,
	       MUReserved5,
	       NoiseFloor,
	       MUReserved6,
	       DataRate,
	       MPDUFlags,
	       MUMac,
	       FrameControl,
	       SequenceControl,
	       MUReserved7|MUList],
    build_mu_list(APMac, DevMacBase, MUListX, Count - 1).



open System;
open System.Runtime.InteropServices
open System.Text.RegularExpressions

let finalTest = "116	1259	1045	679	1334	157	277	1217	218	641	1089	136	247	1195	239	834
269	1751	732	3016	260	6440	5773	4677	306	230	6928	7182	231	2942	2738	3617
644	128	89	361	530	97	35	604	535	297	599	121	567	106	114	480
105	408	120	363	430	102	137	283	123	258	19	101	181	477	463	279
873	116	840	105	285	238	540	22	117	125	699	953	920	106	113	259
3695	161	186	2188	3611	2802	157	2154	3394	145	2725	1327	3741	2493	3607	4041
140	1401	110	119	112	1586	125	937	1469	1015	879	1798	122	1151	100	926
2401	191	219	607	267	2362	932	2283	889	2567	2171	2409	1078	2247	2441	245
928	1142	957	1155	922	1039	452	285	467	305	506	221	281	59	667	232
3882	1698	170	5796	2557	173	1228	4630	174	3508	5629	4395	180	5100	2814	2247
396	311	223	227	340	313	355	469	229	162	107	76	363	132	453	161
627	1331	1143	1572	966	388	198	2068	201	239	176	1805	1506	1890	1980	1887
3390	5336	1730	4072	5342	216	3823	85	5408	5774	247	5308	232	256	5214	787
176	1694	1787	1586	3798	4243	157	4224	3603	2121	3733	851	2493	4136	148	153
2432	4030	3397	4032	3952	2727	157	3284	3450	3229	4169	3471	4255	155	127	186
919	615	335	816	138	97	881	790	855	89	451	789	423	108	95	116"

let test = "5 1 9 5
7 5 3
2 4 6 8"

let getMinMaxDiff (values: int[]) = Array.max values - Array.min values

let splitRow (row:string) = row.Split [|'\t'; ' '; '\r'|]

let getRowSum (row: string) = 
    splitRow row
    |> Array.map Int32.Parse
    |> getMinMaxDiff    

let main (input: string) = 
    input.Split [|'\n'|]
    |> Array.sumBy getRowSum


let isMultiple (x: int) (y: int) = 
    match x with 
    | _ when x % y = 0 -> (true, x/y)
    | _ when y % x = 0 -> (true, y/x)
    | _ -> (false, -1)

let rec hasMultiple (num: int) (nums: int list) = 
    match nums with 
    | [] -> (false, -1)
    | head::tail -> 
        match isMultiple num head with
        | (true, v) -> (true, v)
        | (false, _) -> hasMultiple num tail

let rec findMultipleForNumInList (num: int) (nums: int list) =
    match hasMultiple num nums with
    | (false, _) -> findMultipleForNumInList nums.Head nums.Tail
    | (true, v) -> v

let findMultiples (numbers: int list) = 
    let head::tail = numbers
    findMultipleForNumInList head tail


let getRowMultiples (row: string) = 
    splitRow row
    |> Array.toList
    |> List.map Int32.Parse
    |> findMultiples

let main2 (input: string) = 
    input.Split [|'\n'|]
    |> Array.sumBy getRowMultiples
    
# CodeBattle AI

*   [codebattle-server][20]
*   [codebattle-proto][21]
*   [codebattle-ai][22]
*   [codebattle-client][23]


CodeBattle 这个项目诞生的初衷就是让程序员写AI控制游戏场景中的角色，进行对战。

这里提供一些AI的例子，你可以参考这里的例子来完成自己NB的AI，在写自己AI的时候，可以把这些例子AI作为对手，当你的AI战胜示例AI后，你就可以和你的好基友一起来一场AI对战。

## 基本概念和流程

你可能玩过 一些其他的AI对战游戏，但他们大多数是回合制的。 Codebattle是你控制marine(机枪兵) 与敌人AI的marine进行对战，是实时的。
别人不会等你操作。

1.  首先开启 [Client][1] 端， 创建一个房间。你会得到一个room id
2.  你的AI通过room id加入房间，目前一个房间只支持两个AI对战。
3.  加入房间后会随机的为你创建两个marine
4.  另一个AI通用通过此room id加入房间，此时你们双方都会收到startbattle的消息。
5.  最后尽情的开虐吧。

#### 地图尺寸

加入房间后 会得到 地图size，{x, z}。 （目前创建房间不可选地图，所以x, z固定为50个单位）。

    y     z
    |   /
    |  /
    | /
    |/____________ x 
    0

虽然marine只能在平面中移动，在游戏是三维世界， 所以还是按照 三维坐标来表示地图大小， 地图在 xz 平面， 所以用 x, z来表示地图大小。

x=0, z=0为地图 左下角， x=50, z=50 为地图右上角。

#### Marine

没错就是 星际争霸 中的Marine，（我建模的的时候就是参考这个做的）。
当成功joinroom后，会自动为你创建两个marine，并且将其信息放入 JoinRoomResponse 中返回。

对marine可进行下面的操作：

*   Run， 让其朝某个坐标点{x， z}移动
*   Flares，发射照明弹来获取敌人的状态。 （详细信息见下面的游戏规则）
*   GunAttack， 朝某个坐标点 {x, z}发射子弹。
*   Idle， 静止不动。

##### Marine的移动速度为每秒5个单位
##### Marine发射的子弹移动速度为每秒20个单位。
##### Marine初始生命值100，被GunAttack命中一次生命值减10. 生命值为0时 死亡。


## 游戏规则

当玩家都加入房间后，server发出startbattle指令，游戏开始。

*   游戏初始你只知道自己Marine的状态。并不知道敌人的状态。

*   你可以发射照明弹（Flares）来照亮整个场景，从而获取到所有Marines的状态。

    发射照明弹的瞬间会获取一次状态，并且在1秒后server会再次将所有marine的状态发送给你。

    一旦你发射照明弹，你的状态也被暴露了，其他AI也就知道了你正在发射Flares的marine的状态。但1s后，你会继续获取整个场景中的状态，此时其他AI并不会得到你的状态。

    每个marine的 Flares是有数量限制的。 每个marine最有10个照明弹。

*   当你获取到敌人位置后，你可以进行 GunAttack

    GunAttack 只能指定 坐标点。

    一旦你进行了GunAttack, 你的状态就被暴露一次。其他AI会得到你的状态。

    GunAttack有冷却，间隔2秒才能进行下一次攻击。


*   同样，敌人进行 Flares 或者 GunAttack 的时候，你便知道了此Marine此时的状态。

*   当子弹命中一个marine时， 这个子弹的发射者，以及被命中者，都会对所有的AI暴露一次自己的状态。

*   你的Run， Idle， 都不会将自身暴露，也就是只有通过 Flares 来主动获取所有marine的状态，或者敌人 Flares，GunAttack， 或者子弹命中marine，你被动获取到marine的信息，其他时候你都不知道场景中产生了什么变化。

#### 如果对这个规则还不是很了解，那你可以先看看 [惊爆游戏][2]

## 胜负判定


*   如果游戏中某AI断开了链接，那么游戏会立即终止。没有获胜方。

*   如果某一方的两个marine率先全部死亡，那么另一方获胜。

*   一局对战的时间限制为10分钟，如果在10分钟内没有任何一方的marine全部死亡，那么游戏终止，没有获胜方。 （因为你们的AI要么都足够烂，要么都足够NB）


## 交互数据
AI 与 server 交互的数据使用 google protobuf 进行 encode 和 decode.

CodeBattle 所用的proto文件 在 [这里][3]。

proto 消息在 上面的链接里有说明，这里说一下数据到底如何发送和接收。

#### 每一个完整的数据包，都在首部包含一个4 bytes的数据头，用来表明这个数据包，除过这4 byets的头部后，还有多少字节。

举个例子， 你组装好了一个Api.Cmd命令，变量A 是此命令序列化后的binary。

你首先得到A的长度，LenA, 然后将integer LenA 转换成 4 bytes 长度的binary B， **使用大端序** 。

然后再将 B 和 A 链接起来， C = B + A，

最后将C通过socket发送给server。 

同样，server返回的数据 也包含了 4 bytes 长度的数据头。 你首先从socket读4个字节，然后将大端序的二进制转换成本地整数。然后再次从socket中读取这个整数长度的数据，这样就得到了一个完整的数据包。 最后再用 Api.Message 来解析这个包即可。



[1]: https://github.com/yueyoum/codebattle-client
[2]: http://www.soku.com/detail/show/XMTA4MDI2OA==
[3]: https://github.com/yueyoum/codebattle-proto
[20]: https://github.com/yueyoum/codebattle-server
[21]: https://github.com/yueyoum/codebattle-proto
[22]: https://github.com/yueyoum/codebattle-ai
[23]: https://github.com/yueyoum/codebattle-client
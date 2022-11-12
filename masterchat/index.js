const { Masterchat } = require('masterchat')

const main = async () => {
  const firstArg = process.argv[2]

  if (firstArg === 'TEST') {
    console.log(
      JSON.stringify({
        content: 'dummy content',
        authorName: 'dummy authorName',
        authorChannelId: 'dummy authorChannelId',
      })
    )
    return
  }

  const mc = await Masterchat.init(firstArg)

  const chats = mc.iter().filter((action) => action.type === 'addChatItemAction')

  for await (const chat of chats) {
    console.log(
      JSON.stringify({
        content: chat.message?.map((r) => r.text).join('') ?? '',
        authorName: chat.authorName,
        authorChannelId: chat.authorChannelId,
      })
    );
  }
}

main()

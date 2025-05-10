import { LinearClient, Issue } from "@linear/sdk";
const LINEAR_API_KEY = Bun.env.LINEAR_API_KEY;

const linearClient = new LinearClient({
  apiKey: LINEAR_API_KEY,
});

async function getMyIssues() {
  const me = await linearClient.viewer;
  const myIssues = await me.assignedIssues({
    filter: {
      state: { type: { neq: "completed" } },
      cycle: { isActive: { eq: true } },
    },
  });
  if (myIssues.nodes.length) {
    return Promise.all(myIssues.nodes.map(toOrgNode));
  } else {
    console.log(`${me.displayName} has no issues`);
    return [];
  }
}

async function toOrgNode({
  title,
  description,
  createdAt,
  state,
  cycle,
  url,
  identifier,
  parent,
}: Issue) {
  const parentIssue = await parent;

  return {
    title,
    description: formatDescription(description),
    createdAt,
    state: (await state)?.name,
    deadline: (await cycle)?.endsAt,
    scheduled: (await cycle)?.startsAt,
    url,
    identifier,
    parentId: parentIssue ? parentIssue.identifier : null,
  };
}

await getMyIssues()
  .then((myIssueStates) => {
    const formatedIssues = JSON.stringify(myIssueStates, null, 2);
    console.log(`Successfully fetched my issues: ${formatedIssues}`);

    Bun.write("linear-output.json", formatedIssues);
    console.log('Wrote "linear-output.json" file');
    process.exit(0);
  })
  .catch((e) => {
    console.log(`Error while fetching issues: ${e}`);
    process.exit(1);
  });

function formatDescription(description: string | undefined) {
  return description?.replaceAll(/^\*(\s*)/gm, " *$1");
}

import { openai } from "@ai-sdk/openai";
import { streamText } from "ai";
import { SYSTEM_PROMPT } from "@/lib/ai/system-prompt";

export const maxDuration = 30;

export async function POST(req: Request) {
  const { messages } = await req.json();

  // If no API key, return a helpful message
  if (!process.env.OPENAI_API_KEY) {
    return new Response(
      JSON.stringify({
        error: "no_api_key",
        message:
          "No OpenAI API key configured. Use the guided wizard to build your analysis, or add OPENAI_API_KEY to your .env file to enable AI chat.",
      }),
      { status: 200, headers: { "Content-Type": "application/json" } }
    );
  }

  const result = streamText({
    model: openai("gpt-4o"),
    system: SYSTEM_PROMPT,
    messages,
  });

  return result.toDataStreamResponse();
}

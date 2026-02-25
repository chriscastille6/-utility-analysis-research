import { google } from "@ai-sdk/google";
import { streamText } from "ai";
import { SYSTEM_PROMPT } from "@/lib/ai/system-prompt";

export const maxDuration = 30;

export async function POST(req: Request) {
  const { messages } = await req.json();

  if (!process.env.GOOGLE_GENERATIVE_AI_API_KEY) {
    return new Response(
      JSON.stringify({
        error: "no_api_key",
        message:
          "No Gemini API key configured. Get a free key at https://aistudio.google.com/apikey and add GOOGLE_GENERATIVE_AI_API_KEY to your .env file.",
      }),
      { status: 200, headers: { "Content-Type": "application/json" } }
    );
  }

  const result = streamText({
    model: google("gemini-2.5-flash"),
    system: SYSTEM_PROMPT,
    messages,
  });

  return result.toTextStreamResponse();
}

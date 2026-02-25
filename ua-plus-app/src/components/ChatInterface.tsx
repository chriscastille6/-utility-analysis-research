"use client";

import { useState, useRef, useEffect, useCallback } from "react";
import type { CombinedResult, InterventionParams } from "@/lib/models/utility-engine";
import { formatCurrency } from "@/lib/stats";

interface Props {
  onConfigGenerated: (config: { interventions: Partial<InterventionParams>[]; overlapFactor?: number }) => void;
  results: CombinedResult | null;
}

interface Message {
  role: "user" | "assistant";
  content: string;
}

export function ChatInterface({ onConfigGenerated, results }: Props) {
  const [messages, setMessages] = useState<Message[]>([
    {
      role: "assistant",
      content:
        "Hi! I'm your utility analysis assistant. Tell me about the HR decision you're evaluating and I'll help you estimate the ROI.\n\nFor example, you could say:\n- \"We're thinking about switching to structured interviews for 200 hires\"\n- \"I want to estimate the value of a sales training program\"\n- \"What would be the ROI of implementing goal setting plus leadership development?\"",
    },
  ]);
  const [input, setInput] = useState("");
  const [isLoading, setIsLoading] = useState(false);
  const [noApiKey, setNoApiKey] = useState(false);
  const scrollRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    scrollRef.current?.scrollTo({ top: scrollRef.current.scrollHeight, behavior: "smooth" });
  }, [messages]);

  // Parse analysis JSON from assistant response
  const parseAnalysisConfig = useCallback(
    (text: string) => {
      const match = text.match(/```json:analysis\s*\n([\s\S]*?)\n```/);
      if (!match) return;
      try {
        const config = JSON.parse(match[1]);
        if (config.interventions) {
          onConfigGenerated(config);
        }
      } catch {
        // JSON parse failed — no config to extract
      }
    },
    [onConfigGenerated]
  );

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!input.trim() || isLoading) return;

    const userMessage: Message = { role: "user", content: input };
    const newMessages = [...messages, userMessage];
    setMessages(newMessages);
    setInput("");
    setIsLoading(true);

    // Add context about current results if available
    let contextMessages = newMessages.map((m) => ({ role: m.role, content: m.content }));
    if (results) {
      const ctx = `[System context: The user currently has ${results.individualResults.length} intervention(s) configured with a combined net benefit of ${formatCurrency(results.combinedNetBenefit)} and ROI of ${results.combinedROI.toFixed(1)}:1. Individual interventions: ${results.individualResults.map((r) => `${r.interventionLabel} (net: ${formatCurrency(r.netBenefit)}, effect size: ${r.effectSizeUsed.toFixed(2)})`).join(", ")}]`;
      contextMessages = [{ role: "user" as const, content: ctx }, ...contextMessages];
    }

    try {
      const res = await fetch("/api/chat", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ messages: contextMessages }),
      });

      if (!res.ok) throw new Error("API error");

      const contentType = res.headers.get("content-type") ?? "";

      if (contentType.includes("application/json")) {
        const data = await res.json();
        if (data.error === "no_api_key") {
          setNoApiKey(true);
          setMessages((prev) => [
            ...prev,
            {
              role: "assistant",
              content:
                "I need an OpenAI API key to chat. You can add `OPENAI_API_KEY` to your `.env` file. In the meantime, use the **Build Analysis** tab to configure interventions with the guided wizard — it works great without AI!",
            },
          ]);
          setIsLoading(false);
          return;
        }
      }

      // Stream the response
      const reader = res.body?.getReader();
      if (!reader) throw new Error("No reader");

      let assistantContent = "";
      setMessages((prev) => [...prev, { role: "assistant", content: "" }]);

      const decoder = new TextDecoder();
      while (true) {
        const { done, value } = await reader.read();
        if (done) break;
        const chunk = decoder.decode(value, { stream: true });
        // Parse SSE data lines from AI SDK
        const lines = chunk.split("\n");
        for (const line of lines) {
          if (line.startsWith("0:")) {
            try {
              const text = JSON.parse(line.slice(2));
              assistantContent += text;
              setMessages((prev) => {
                const updated = [...prev];
                updated[updated.length - 1] = { role: "assistant", content: assistantContent };
                return updated;
              });
            } catch {
              // skip unparseable lines
            }
          }
        }
      }

      parseAnalysisConfig(assistantContent);
    } catch {
      setMessages((prev) => [
        ...prev,
        {
          role: "assistant",
          content: "Sorry, I had trouble connecting. Please try again or use the guided wizard in the Build Analysis tab.",
        },
      ]);
    }

    setIsLoading(false);
  };

  return (
    <div className="flex flex-col h-full">
      <div className="px-4 py-3 border-b border-[var(--border)]">
        <h2 className="font-semibold text-sm">AI Assistant</h2>
        <p className="text-xs text-[var(--muted)]">Describe your HR decision in plain language</p>
      </div>

      {/* Messages */}
      <div ref={scrollRef} className="flex-1 overflow-auto p-4 space-y-4">
        {messages.map((msg, i) => (
          <div key={i} className={`flex ${msg.role === "user" ? "justify-end" : "justify-start"}`}>
            <div
              className={`max-w-[85%] rounded-lg px-3 py-2 text-sm whitespace-pre-wrap ${
                msg.role === "user"
                  ? "bg-[var(--primary)] text-white"
                  : "bg-[var(--surface)] text-[var(--foreground)]"
              }`}
            >
              {msg.content}
            </div>
          </div>
        ))}
        {isLoading && (
          <div className="flex justify-start">
            <div className="bg-[var(--surface)] rounded-lg px-3 py-2 text-sm text-[var(--muted)]">
              Thinking...
            </div>
          </div>
        )}
      </div>

      {/* Input */}
      <form onSubmit={handleSubmit} className="p-4 border-t border-[var(--border)]">
        <div className="flex gap-2">
          <input
            type="text"
            value={input}
            onChange={(e) => setInput(e.target.value)}
            placeholder={noApiKey ? "Add OPENAI_API_KEY to enable chat..." : "Describe your HR decision..."}
            className="input-field text-sm"
            disabled={isLoading || noApiKey}
          />
          <button type="submit" className="btn-primary text-sm whitespace-nowrap" disabled={isLoading || noApiKey}>
            Send
          </button>
        </div>
      </form>
    </div>
  );
}

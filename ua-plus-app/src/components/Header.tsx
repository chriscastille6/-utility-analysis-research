"use client";

export function Header({ onToggleChat, showChat }: { onToggleChat: () => void; showChat: boolean }) {
  return (
    <header className="bg-white border-b border-[var(--border)] px-6 py-3 flex items-center justify-between">
      <div className="flex items-center gap-3">
        <div className="w-8 h-8 rounded-lg bg-[var(--primary)] flex items-center justify-center text-white font-bold text-sm">
          UA+
        </div>
        <div>
          <h1 className="text-lg font-semibold text-[var(--foreground)]">Utility Analysis+</h1>
          <p className="text-xs text-[var(--muted)]">Evidence-based ROI for HR decisions</p>
        </div>
      </div>

      <div className="flex items-center gap-3">
        <button
          onClick={onToggleChat}
          className={`flex items-center gap-2 px-3 py-2 rounded-lg text-sm font-medium transition-colors ${
            showChat
              ? "bg-[var(--primary)] text-white"
              : "bg-[var(--surface)] text-[var(--foreground)] hover:bg-[var(--border)]"
          }`}
        >
          <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
            <path d="M21 15a2 2 0 0 1-2 2H7l-4 4V5a2 2 0 0 1 2-2h14a2 2 0 0 1 2 2z" />
          </svg>
          AI Assistant
        </button>
      </div>
    </header>
  );
}

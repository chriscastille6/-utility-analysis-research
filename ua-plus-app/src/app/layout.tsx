import type { Metadata } from "next";
import "./globals.css";

export const metadata: Metadata = {
  title: "UA+ | Utility Analysis Platform",
  description:
    "AI-powered utility analysis for HR decisions. Estimate the ROI of selection, training, and organizational interventions using meta-analytic evidence.",
};

export default function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    <html lang="en">
      <body className="antialiased">{children}</body>
    </html>
  );
}

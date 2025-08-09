import type { ReactNode } from "react";
import Header from "../Components/Header"
import Footer from "../Components/Footer";

export default function Layout({ children }: { children: ReactNode }) {
  return (
    <div className="min-h-screen flex flex-col bg-black text-white items-center pt-5">
      <Header />
      <main className="flex-grow flex items-center justify-center p-6">
        {children}
      </main>
      <Footer />
    </div>
  );
}

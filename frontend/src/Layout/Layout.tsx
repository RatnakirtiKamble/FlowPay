import type { ReactNode } from "react";
import { useState } from "react";
import { Header } from "../Components/Header";
import { AuthModal, type AuthMode } from "../Modals/Auth";
import Footer from "../Components/Footer";

export default function Layout({ children }: { children: ReactNode }) {
  const [modalOpen, setModalOpen] = useState(false);
  const [mode, setMode] = useState<AuthMode>("login");

  const openModal = (mode: AuthMode) => {
    setMode(mode);
    setModalOpen(true);
  };

  const onModeChange = (newMode: AuthMode) => {
    setMode(newMode);
  };

  const closeModal = () => {
    setModalOpen(false);
  };

  // Optionally track logged-in user if you want here
  // const [userEmail, setUserEmail] = useState<string | null>(null);
  // const handleLoginSuccess = (email: string) => setUserEmail(email);

  return (
    <div className="min-h-screen flex flex-col bg-black text-white items-center pt-5">
      <Header onLoginClick={() => openModal("login")} onSignupClick={() => openModal("signup")} />

      <main className="flex-grow flex items-center justify-center p-6 w-full max-w-7xl bg-gradient-to-r from-black via-gray-900 to-black ">
        {children}
      </main>

      <Footer />

      {modalOpen && (
        <AuthModal
          mode={mode}
          onClose={closeModal}
                    onLoginSuccess={(email) => {
                      closeModal();
                      // Optional: handle login success globally here
                      // setUserEmail(email);
                    }}
                    onModeChange={onModeChange}
                  />
                )}
              </div>
            );
          }

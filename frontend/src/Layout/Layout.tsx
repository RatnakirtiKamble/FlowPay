import type { ReactNode } from "react";
import { Header } from "../Components/Header";
import { AuthModal } from "../Modals/Auth";
import Footer from "../Components/Footer";
import { useAuth } from "../Context/AuthContext";

export default function Layout({ children }: { children: ReactNode }) {
  // Get all modal-related state and functions from the central AuthContext
  const { 
    authModalMode, 
    openLoginModal, 
    openSignupModal, 
    closeAuthModal 
  } = useAuth();

  const onModeChange = (newMode: 'login' | 'signup') => {
    if (newMode === 'login') {
      openLoginModal();
    } else {
      openSignupModal();
    }
  };

  return (
    <div className="min-h-screen flex flex-col bg-gradient-to-r from-black via-gray-900 to-black text-white items-center pt-5">
      {/* The Header now gets its functions directly from the context */}
      <Header onLoginClick={openLoginModal} onSignupClick={openSignupModal} />

      <main className="flex-grow w-full p-6 max-w-7xl mx-auto min-h-0">
        {children}
      </main>

      <Footer />

      {/* The AuthModal's visibility and mode are now controlled by the context */}
      {authModalMode && (
        <AuthModal
          mode={authModalMode}
          onClose={closeAuthModal}
          onModeChange={onModeChange}
        />
      )}
    </div>
  );
}

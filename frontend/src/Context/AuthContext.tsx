import { createContext, useState, useEffect, useContext, type ReactNode } from "react";
import { useNavigate } from "react-router-dom";

// Helper function to read a specific cookie
function getCookie(name: string): string | null {
  const value = `; ${document.cookie}`;
  const parts = value.split(`; ${name}=`);
  if (parts.length === 2) {
    return parts.pop()?.split(";").shift() || null;
  }
  return null;
}

// Interfaces for User and Context Type
interface User {
  name: string;
  email: string;
  balance: number;
  apiKeyExists: boolean;
}

interface AuthContextType {
  user: User | null;
  loading: boolean;
  login: (email: string, password: string) => Promise<void>;
  logout: () => Promise<void>;
  refreshUser: () => Promise<void>;
  // --- Properties for modal control ---
  authModalMode: 'login' | 'signup' | null;
  openLoginModal: () => void;
  openSignupModal: () => void;
  closeAuthModal: () => void;
}

const AuthContext = createContext<AuthContextType | undefined>(undefined);

export const AuthProvider = ({ children }: { children: ReactNode }) => {
  const [user, setUser] = useState<User | null>(null);
  const [loading, setLoading] = useState(true);
  // --- State for managing the modal ---
  const [authModalMode, setAuthModalMode] = useState<'login' | 'signup' | null>(null);
  const navigate = useNavigate();

  // --- Functions to control the modal state ---
  const openLoginModal = () => setAuthModalMode('login');
  const openSignupModal = () => setAuthModalMode('signup');
  const closeAuthModal = () => setAuthModalMode(null);

  const refreshUser = async () => {
    try {
      const csrfToken = getCookie("XSRF-TOKEN");
      const response = await fetch(`${import.meta.env.VITE_BACKEND_URL}/dashboard`, {
        credentials: "include",
        headers: { "X-XSRF-TOKEN": csrfToken || ""},
      });
      
      if (response.ok) {
        const dashboardData = await response.json();
        const loggedInUser: User = {
          name: dashboardData.publicMerchantName,
          email: dashboardData.publicMerchantEmail,
          balance: dashboardData.publicMerchantBalance,
          apiKeyExists: dashboardData.publicMerchantApiKeyExists,
        };
        setUser(loggedInUser);
      } else {
        setUser(null);
      }
    } catch (error) {
      setUser(null);
    }
  };

  useEffect(() => {
    const checkUserSession = async () => {
      await refreshUser();
      setLoading(false);
    };
    checkUserSession();
  }, []);

  const login = async (email: string, password: string) => {
    setLoading(true);
    try {
      const loginRes = await fetch(`${import.meta.env.VITE_BACKEND_URL}/login`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        credentials: "include",
        body: JSON.stringify({ loginEmail: email, loginPassword: password }),
      });

      if (!loginRes.ok) {
        const errorData = await loginRes.json().catch(() => ({}));
        throw new Error(errorData.message || "Invalid credentials");
      }
      
      await refreshUser(); 
      navigate("/dashboard");

    } finally {
      setLoading(false);
    }
  };
  
  const logout = async () => {
    try {
        const csrfToken = getCookie("XSRF-TOKEN");
        await fetch(`${import.meta.env.VITE_BACKEND_URL}/logout`, {
            method: "POST",
            credentials: "include",
            headers: {
                "X-XSRF-TOKEN": csrfToken || "",
            },
        });
    } catch (error) {
        console.error("Logout API call failed:", error);
    } finally {
        setUser(null);
        navigate("/");
    }
  };

  const contextValue = {
    user,
    loading,
    login,
    logout,
    refreshUser,
    authModalMode,
    openLoginModal,
    openSignupModal,
    closeAuthModal
  };

  return (
    <AuthContext.Provider value={contextValue}>
      {children}
    </AuthContext.Provider>
  );
};

export const useAuth = () => {
  const context = useContext(AuthContext);
  if (context === undefined) {
    throw new Error("useAuth must be used within an AuthProvider");
  }
  return context;
};

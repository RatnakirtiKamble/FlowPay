import { createContext, useState, useEffect, useContext, useMemo, type ReactNode } from "react";
import { useNavigate } from "react-router-dom";
import axios from "axios";

// ====================================================================================
// 1. TYPE DEFINITIONS
// ====================================================================================
interface PublicMerchant {
  publicMerchantName: string;
  publicMerchantEmail: string;
  publicMerchantBalance: number;
  publicMerchantApiKeyExists: boolean;
}

interface LoginResponse {
  lrAccessToken: string;
  lrXsrfToken: string;
  lrMerchant: PublicMerchant;
}

interface AuthContextType {
  user: PublicMerchant | null;
  accessToken: string | null;
  loading: boolean;
  login: (email: string, password: string) => Promise<void>;
  logout: () => void;
  updateUser: (updatedFields: Partial<PublicMerchant>) => void;
  refreshUser: () => Promise<void>;
  authModalMode: "login" | "signup" | null;
  openLoginModal: () => void;
  openSignupModal: () => void;
  closeAuthModal: () => void;
}

// ====================================================================================
// 2. CONFIGURED API CLIENT
// ====================================================================================
export const apiClient = axios.create({
  baseURL: import.meta.env.VITE_BACKEND_URL,
});

// ====================================================================================
// 3. CONTEXT CREATION
// ====================================================================================
const AuthContext = createContext<AuthContextType | undefined>(undefined);

// ====================================================================================
// 4. AUTH PROVIDER COMPONENT
// ====================================================================================
export const AuthProvider = ({ children }: { children: ReactNode }) => {
  const [user, setUser] = useState<PublicMerchant | null>(null);
  const [accessToken, setAccessToken] = useState<string | null>(null);
  const [loading, setLoading] = useState(true);
  const [authModalMode, setAuthModalMode] = useState<"login" | "signup" | null>(null);
  const navigate = useNavigate();

  // --------------------------------------------------------------------
  // Load session from localStorage on initial mount
  // --------------------------------------------------------------------
  useEffect(() => {
    try {
      const storedToken = localStorage.getItem("accessToken");
      const storedUser = localStorage.getItem("user");
      if (storedToken && storedUser) {
        setAccessToken(storedToken);
        setUser(JSON.parse(storedUser));
      }
    } catch (error) {
      console.error("Failed to parse stored auth data", error);
    } finally {
      setLoading(false);
    }
  }, []);

  // --------------------------------------------------------------------
  // Attach auth headers to every Axios request if user is logged in
  // --------------------------------------------------------------------
  useEffect(() => {
    const requestInterceptor = apiClient.interceptors.request.use(
      (config) => {
        const xsrfToken = localStorage.getItem("xsrfToken");
        if (accessToken) {
          config.headers["Authorization"] = `Bearer ${accessToken}`;
          if (xsrfToken) config.headers["X-XSRF-TOKEN"] = xsrfToken;
        }
        return config;
      },
      (error) => Promise.reject(error)
    );
    return () => apiClient.interceptors.request.eject(requestInterceptor);
  }, [accessToken]);

  // --------------------------------------------------------------------
  // Modal control helpers
  // --------------------------------------------------------------------
  const openLoginModal = () => setAuthModalMode("login");
  const openSignupModal = () => setAuthModalMode("signup");
  const closeAuthModal = () => setAuthModalMode(null);

  // --------------------------------------------------------------------
  // Update only specific fields of the user object
  // --------------------------------------------------------------------
  const updateUser = (updatedFields: Partial<PublicMerchant>) => {
    setUser((prevUser) => {
      if (!prevUser) return null;
      const newUser = { ...prevUser, ...updatedFields };
      localStorage.setItem("user", JSON.stringify(newUser));
      return newUser;
    });
  };

  // --------------------------------------------------------------------
  // Perform login and store credentials in localStorage
  // --------------------------------------------------------------------
  const login = async (email: string, password: string) => {
    const response = await apiClient.post<LoginResponse>("/login", {
      loginEmail: email,
      loginPassword: password,
    });
    const { lrAccessToken, lrXsrfToken, lrMerchant } = response.data;
    localStorage.setItem("accessToken", lrAccessToken);
    localStorage.setItem("xsrfToken", lrXsrfToken);
    localStorage.setItem("user", JSON.stringify(lrMerchant));
    setAccessToken(lrAccessToken);
    setUser(lrMerchant);
  };

  // --------------------------------------------------------------------
  // Clear session and navigate to home
  // --------------------------------------------------------------------
  const logout = () => {
    setAccessToken(null);
    setUser(null);
    localStorage.removeItem("accessToken");
    localStorage.removeItem("xsrfToken");
    localStorage.removeItem("user");
    navigate("/");
  };

  // --------------------------------------------------------------------
  // Fetch latest user data from backend
  // --------------------------------------------------------------------
  const refreshUser = async () => {
    try {
      const response = await apiClient.get<PublicMerchant>("/dashboard");
      updateUser(response.data);
    } catch (error) {
      console.error("Failed to refresh user data:", error);
      if (axios.isAxiosError(error) && error.response?.status === 401) {
        logout();
      }
    }
  };

  // --------------------------------------------------------------------
  // Memoized context value for performance
  // --------------------------------------------------------------------
  const contextValue = useMemo(
    () => ({
      user,
      accessToken,
      loading,
      login,
      logout,
      updateUser,
      refreshUser,
      authModalMode,
      openLoginModal,
      openSignupModal,
      closeAuthModal,
    }),
    [user, accessToken, loading, authModalMode]
  );

  return (
    <AuthContext.Provider value={contextValue}>
      {!loading && children}
    </AuthContext.Provider>
  );
};

// ====================================================================================
// 5. HOOK FOR CONSUMERS
// ====================================================================================
export const useAuth = () => {
  const context = useContext(AuthContext);
  if (context === undefined)
    throw new Error("useAuth must be used within an AuthProvider");
  return context;
};

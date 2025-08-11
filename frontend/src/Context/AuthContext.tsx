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
  refreshUser: () => Promise<void>; // ++ We will implement this
  authModalMode: 'login' | 'signup' | null;
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

const AuthContext = createContext<AuthContextType | undefined>(undefined);

export const AuthProvider = ({ children }: { children: ReactNode }) => {
  const [user, setUser] = useState<PublicMerchant | null>(null);
  const [accessToken, setAccessToken] = useState<string | null>(null);
  const [loading, setLoading] = useState(true);
  const [authModalMode, setAuthModalMode] = useState<'login' | 'signup' | null>(null);
  const navigate = useNavigate();

  // --- Session Persistence from localStorage ---
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

  // --- Axios Interceptor for Auth Headers ---
  useEffect(() => {
    const requestInterceptor = apiClient.interceptors.request.use(
      (config) => {
        const xsrfToken = localStorage.getItem("xsrfToken");
        if (accessToken) {
          config.headers['Authorization'] = `Bearer ${accessToken}`;
          if (xsrfToken) config.headers['X-XSRF-TOKEN'] = xsrfToken;
        }
        return config;
      },
      (error) => Promise.reject(error)
    );
    return () => apiClient.interceptors.request.eject(requestInterceptor);
  }, [accessToken]);

  const openLoginModal = () => setAuthModalMode('login');
  const openSignupModal = () => setAuthModalMode('signup');
  const closeAuthModal = () => setAuthModalMode(null);

  // --- LOGIN/LOGOUT & USER UPDATE LOGIC ---
  const updateUser = (updatedFields: Partial<PublicMerchant>) => {
    setUser(prevUser => {
      if (!prevUser) return null;
      const newUser = { ...prevUser, ...updatedFields };
      localStorage.setItem("user", JSON.stringify(newUser));
      return newUser;
    });
  };

  const login = async (email: string, password: string) => {
    const response = await apiClient.post<LoginResponse>('/login', {
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
  
  const logout = () => {
    setAccessToken(null);
    setUser(null);
    localStorage.removeItem("accessToken");
    localStorage.removeItem("xsrfToken");
    localStorage.removeItem("user");
    navigate("/");
  };

  // ++ NEW: A function to fetch the latest user data from the /dashboard endpoint
  const refreshUser = async () => {
    try {
      const response = await apiClient.get<PublicMerchant>('/dashboard');
      updateUser(response.data); // Update the user state with the fresh data
    } catch (error) {
      console.error("Failed to refresh user data:", error);
      // If the token is expired or invalid, log the user out
      if (axios.isAxiosError(error) && error.response?.status === 401) {
        logout();
      }
    }
  };

  const contextValue = useMemo(() => ({
    user, accessToken, loading, login, logout, updateUser, refreshUser, // ++ Expose refreshUser
    authModalMode, openLoginModal, openSignupModal, closeAuthModal
  }), [user, accessToken, loading, authModalMode]);

  return (
    <AuthContext.Provider value={contextValue}>
      {!loading && children}
    </AuthContext.Provider>
  );
};

export const useAuth = () => {
  const context = useContext(AuthContext);
  if (context === undefined) throw new Error("useAuth must be used within an AuthProvider");
  return context;
};

import React, { useState, type FormEvent, type MouseEvent } from "react";

export type AuthMode = "login" | "signup";

interface AuthModalProps {
    mode: AuthMode;
    onClose: () => void;
    onLoginSuccess: (email: string) => void;
    onModeChange: (mode: AuthMode) => void;
  }
  

interface RegisterRequest {
  reqName: string;
  reqEmail: string;
  reqPassword: string;
}

interface LoginRequest {
  loginEmail: string;
  loginPassword: string;
}

export const AuthModal: React.FC<AuthModalProps> = ({ mode, onClose, onLoginSuccess, onModeChange }) => {
  const [name, setName] = useState<string>("");
  const [email, setEmail] = useState<string>("");
  const [password, setPassword] = useState<string>("");
  const [confirmPassword, setConfirmPassword] = useState<string>("");
  const [error, setError] = useState<string | null>(null);
  const [loading, setLoading] = useState<boolean>(false);

  const isSignup = mode === "signup";

  const loginUser = async (email: string, password: string): Promise<void> => {
    const res = await fetch("http://localhost:8080/login", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      credentials: "include",
      body: JSON.stringify({ loginEmail: email, loginPassword: password } as LoginRequest),
    });

    if (!res.ok) {
      const data = await res.json().catch(() => ({}));
      throw new Error(data.message || "Login failed");
    }
    console.log(email)
    onLoginSuccess(email);
  };

  const handleSubmit = async (e: FormEvent<HTMLFormElement>): Promise<void> => {
    e.preventDefault();
    setError(null);

    if (isSignup && password !== confirmPassword) {
      setError("Passwords do not match");
      return;
    }

    setLoading(true);

    try {
      if (isSignup) {
        const registerBody: RegisterRequest = {
          reqName: name,
          reqEmail: email,
          reqPassword: password,
        };

        const res = await fetch("http://localhost:8080/register", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          credentials: "include",
          body: JSON.stringify(registerBody),
        });

        if (!res.ok) {
          const data = await res.json().catch(() => ({}));
          throw new Error(data.message || "Registration failed");
        }

        // Auto login after successful registration
        await loginUser(email, password);
      } else {
        await loginUser(email, password);
      }

      setLoading(false);
      onClose();
    } catch (err) {
      setError((err as Error).message);
      setLoading(false);
    }
  };

  const stopPropagation = (e: MouseEvent<HTMLDivElement>) => e.stopPropagation();

  return (
    <div
      className="fixed inset-0 bg-black/50 flex justify-center items-center z-50"
      onClick={onClose}
    >
      <div
        className="bg-white rounded-lg p-8 w-96 relative"
        onClick={stopPropagation}
      >
        <h2 className="text-2xl mb-4 text-blue-500 font-medium">{isSignup ? "Sign Up" : "Log In"}</h2>
        <form onSubmit={handleSubmit} className="flex flex-col space-y-4">
          {isSignup && (
            <input
              type="text"
              placeholder="Name"
              required
              value={name}
              onChange={(e) => setName(e.target.value)}
              className="p-2 rounded text-gray-500 bg-gray-200 border-gray-100"
            />
          )}
          <input
            type="email"
            placeholder="Email"
            required
            value={email}
            onChange={(e) => setEmail(e.target.value)}
            className="p-2 rounded text-gray-500 bg-gray-200 border-gray-100"
          />
          <input
            type="password"
            placeholder="Password"
            required
            value={password}
            onChange={(e) => setPassword(e.target.value)}
            className="p-2 rounded text-gray-500 bg-gray-200 border-gray-100"
          />
          {isSignup && (
            <input
              type="password"
              placeholder="Confirm Password"
              required
              value={confirmPassword}
              onChange={(e) => setConfirmPassword(e.target.value)}
              className="p-2 rounded text-gray-500 bg-gray-200 border-gray-100"
            />
          )}
          {error && <p className="text-red-500">{error}</p>}
          <button className="text-blue-500 text-sm" onClick={() => onModeChange(isSignup ? "login" : "signup")}>
            {isSignup
              ? "Already have an account? Log in instead."
              : "Don't have an account? Sign up now!"}
          </button>
          <button
            type="submit"
            disabled={loading}
            className="bg-blue-500 hover:bg-blue-600 p-2 rounded text-white"
          >
            {loading ? "Please wait..." : isSignup ? "Sign Up" : "Log In"}
          </button>
        </form>
        <button
          className="absolute top-2 right-4 text-gray-400 hover:text-white"
          onClick={onClose}
          aria-label="Close modal"
        >
          &#x2715;
        </button>
      </div>
    </div>
  );
};

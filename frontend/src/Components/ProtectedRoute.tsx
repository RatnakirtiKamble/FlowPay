import { Navigate } from "react-router-dom";
import { type ReactNode } from "react";
import { useAuth } from "../Context/AuthContext";

interface ProtectedRouteProps {
  children: ReactNode;
}

function ProtectedRoute({ children }: ProtectedRouteProps) {
  // Get both user and loading from the context
  const { user, loading } = useAuth();

  // 1. If we are in the middle of a login process, show a loading screen.
  if (loading) {
    return (
      <div className="flex justify-center items-center min-h-screen text-white text-2xl">
        Loading...
      </div>
    );
  }

  // 2. If we are done loading and there is NO user, redirect.
  if (!user) {
    return <Navigate to="/" replace />;
  }

  // 3. If we are done loading and there IS a user, show the dashboard.
  return <>{children}</>;
}

export default ProtectedRoute;